import psycopg2
import scipy.stats
import numpy as np
import matplotlib.pyplot as plt
from sklearn import preprocessing
import torch as T
import torch.nn.functional as F
import pandas as pd
import json
import math


def accuracy(model, data_x, data_y, pct_close):
    n_items = len(data_y)
    X = T.Tensor(data_x)  # 2-d Tensor
    Y = T.Tensor(data_y)  # actual as 1-d Tensor
    oupt = model(X)  # all predicted as 2-d Tensor
    pred = oupt.view(n_items)  # all predicted as 1-d
    n_correct = T.sum((T.abs(pred - Y) < T.abs(pct_close * Y)))
    result = (n_correct.item() * 100.0 / n_items)  # scalar
    return result


class Net(T.nn.Module):
    def __init__(self):
        super(Net, self).__init__()
        self.hid1 = T.nn.Linear(1, 10)  # 13-(10-10)-1
        self.hid2 = T.nn.Linear(10, 10)
        self.oupt = T.nn.Linear(10, 1)
        T.nn.init.xavier_uniform_(self.hid1.weight)  # glorot
        T.nn.init.zeros_(self.hid1.bias)
        T.nn.init.xavier_uniform_(self.hid2.weight)
        T.nn.init.zeros_(self.hid2.bias)
        T.nn.init.xavier_uniform_(self.oupt.weight)
        T.nn.init.zeros_(self.oupt.bias)

    def forward(self, x):
        z = T.tanh(self.hid1(x))
        z = T.tanh(self.hid2(z))
        z = self.oupt(z)  # no activation, aka Identity()
        return z


class LinearRegressionModel(T.nn.Module):
    def __init__(self):
        super(LinearRegressionModel, self).__init__()
        self.linear = T.nn.Linear(1, 1)

    def forward(self, x):
        y_pred = self.linear(x)
        return y_pred


class CustomModel(T.nn.Module):
    def __init__(self, n_feature, n_hidden, n_output):
        super(CustomModel, self).__init__()
        self.hidden = T.nn.Linear(n_feature, n_hidden)
        self.predict = T.nn.Linear(n_hidden, n_output)

    def forward(self, x):
        x = F.relu(self.hidden(x))
        x = self.predict(x)
        return x


def load_fundamentals_data(required_attributes):
    def has_all_required_attributes(record):
        if record is None:
            return False

        for attr in required_attributes:
            attr_val = record[attr]
            if attr_val is None or attr_val == "":
                return False
        return True

    with open("../data/usfundamentals.com/latest-snapshot-quarterly-MERGED.json", "r") as text_file:
        # Load json, remove incomplete rows, process into DataFrame
        return pd.DataFrame.from_records(filter(has_all_required_attributes, json.load(text_file)))


def process_fundamentals_data(data, x_cols, y_cols):
    row_count = data.shape[0]
    # Use the first 3/4 of the data as training, and the last 1/4 as test
    split_index = math.floor(row_count * 0.75)
    train_x = data[:split_index][x_cols].values.astype('float64')
    train_y = data[:split_index][y_cols].values.astype('float64')
    test_x = data[split_index:][x_cols].values.astype('float64')
    test_y = data[split_index:][y_cols].values.astype('float64')
    return train_x, train_y, test_x, test_y


def train_model(model, loss_func, optimizer, train_x, train_y, batch_count=5000, batch_size=100, plot_progress=True):
    n_items = len(train_x)

    loss_history = []
    for batch_idx in range(batch_count):
        # Choose a random sample of the data
        curr_batch = np.random.choice(n_items, batch_size, replace=False)
        batch_x = T.Tensor(train_x[curr_batch])
        batch_y = T.Tensor(train_y[curr_batch])  # .view(batch_size, 1)
        # Forward pass - Predict y values based on the actual x values
        prediction_y = model(batch_x)
        # Calculate loss
        loss_obj = loss_func(prediction_y, batch_y)
        loss = loss_obj.item()
        loss_history.append(loss)
        # Zero gradients
        optimizer.zero_grad()
        # Backward pass
        loss_obj.backward()
        # Apply gradients
        optimizer.step()

        # Stop when loss is invalid
        if math.isnan(loss) or math.isinf(loss):
            print("[ERROR] Loss is {}".format(loss))
            break

        if plot_progress and batch_idx % 10 == 0:
            # plot and show learning process
            plt.cla()
            plt.scatter(batch_x.data.numpy(), batch_y.data.numpy())
            plt.scatter(batch_x.data.numpy(), prediction_y.data.numpy(), c='deeppink')
            plt.pause(0.5)

        print('Loss: {:.6f} after {} batches'.format(loss, batch_idx + 1))

    return loss_history


def test_model(model, test_x, test_y):
    prediction_y = model(T.Tensor(test_x))

    plt.close()
    plt.cla()
    plt.scatter(test_x, test_y)
    plt.scatter(test_x, prediction_y.data.numpy(), c='deeppink')
    plt.show()


def plot_loss(history):
    plt.close()
    plt.cla()
    plt.plot(history)
    plt.show()


def compare_attributes(x_cols, y_cols):
    print("COMPARING {} and {}".format(str(x_cols), str(y_cols)))

    # Load data
    print("Loading quarterly fundamentals data...")
    fundamentals_data = load_fundamentals_data(x_cols + y_cols)
    print(fundamentals_data[x_cols + y_cols])

    # Split into train and test
    print("Processing dataset...")
    train_x, train_y, test_x, test_y = process_fundamentals_data(fundamentals_data, x_cols, y_cols)
    train_x = preprocessing.scale(train_x)
    train_y = preprocessing.scale(train_y)
    print(train_x)
    print(train_y)

    # Show training  data
    plt.scatter(train_x, train_y)
    plt.show()

    # Create model
    model = CustomModel(1, 10, 1)  # LinearRegressionModel()
    # Define loss func & optimizer
    loss_func = T.nn.MSELoss()
    optimizer = T.optim.Adam(model.parameters(), lr=0.0001)  # T.optim.SGD(model.parameters(), lr=0.0001)
    # Train model
    loss_history = train_model(model, loss_func, optimizer, train_x, train_y,
                               batch_count=25000, batch_size=100, plot_progress=False)

    # Plot loss
    plot_loss(loss_history)

    # Test model
    test_model(model, test_x, test_y)

    return loss_history[-1]


def main():
    # Initialize seed weights to ensure reproducible results
    T.manual_seed(1)
    np.random.seed(1)

    x_cols = ["Liabilities"]
    y_cols = ["Assets"]

    columns = ["Assets", "AssetsCurrent",
               "CashAndCashEquivalentsAtCarryingValue",
               "Liabilities", "LiabilitiesCurrent",
               "NetCashProvidedByUsedInFinancingActivities",
               "NetCashProvidedByUsedInInvestingActivities",
               "NetCashProvidedByUsedInOperatingActivities",
               "OperatingIncomeLoss",
               "PropertyPlantAndEquipmentNet",
               "Revenues"
               ]

    for col_a in columns:
        for col_b in columns:
            if col_a == col_b:
                continue

            compare_attributes([col_a], [col_b])

    # Explicitly exit because PyTorch sometimes hangs
    exit(0)


"""
# Create model
print("Creating 13-(10-10)-1 DNN regression model")
net = Net()

# Train model
net = net.train()  # set training mode
bat_size = 10
loss_func = T.nn.MSELoss()  # mean squared error
optimizer = T.optim.SGD(net.parameters(), lr=0.01)
n_items = len(train_x)
batches_per_epoch = n_items // bat_size
max_batches = 100 * batches_per_epoch
print("Starting training")
for b in range(max_batches):
    curr_bat = np.random.choice(n_items, bat_size,
                                replace=False)
    X = T.Tensor(train_x[curr_bat])
    Y = T.Tensor(train_y[curr_bat]).view(bat_size,1)
    optimizer.zero_grad()
    oupt = net(X)
    loss_obj = loss_func(oupt, Y)
    loss_obj.backward()
    optimizer.step()
    if b % (max_batches // 10) == 0:
        print("batch = %6d" % b, end="")
        print("  batch loss = %7.4f" % loss_obj.item(), end="")
        net = net.eval()
        acc = accuracy(net, train_x, train_y, 0.15)
        net = net.train()
        print("  accuracy = %0.2f%%" % acc)
print("Training complete \n")

# 4. Evaluate model
net = net.eval()  # set eval mode
acc = accuracy(net, test_x, test_y, 0.15)
print("Accuracy on test data = %0.2f%%" % acc)

# 5. Save model - TODO
# 6. Use model
raw_inpt = np.array([[12], [13], [14]], dtype=np.float32)
X = T.Tensor(raw_inpt)
y = net(X)
#print("Prediction: %0.2f" % y.items())
print(y)
"""

if __name__ == "__main__":
    main()

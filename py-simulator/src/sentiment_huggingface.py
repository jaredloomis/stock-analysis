from transformers import AutoTokenizer, AutoModelForSequenceClassification, TrainingArguments, Trainer

#tokenizer = AutoTokenizer.from_pretrained("nlptown/bert-base-multilingual-uncased-sentiment")
#model = AutoModelForSequenceClassification.from_pretrained("nlptown/bert-base-multilingual-uncased-sentiment")
tokenizer = AutoTokenizer.from_pretrained("bert-base-multilingual-cased")
model = AutoModelForSequenceClassification.from_pretrained("bert-base-multilingual-cased")
model.eval()
tests = ["wasabi is good", "the wheels were nice and shiny", "This is some commie bullshit perputrated by pelosi", "i am a little teapot", "would not recommend to friends", "who do they think they are making such a difficult product", "i can not sleep anymore", "it is a hassle at times but overall do not regret my purchase", "amd new earnings indicate room for growth within the sector"]
for test in tests:
  inputs = tokenizer(test, return_tensors="pt")
  outputs = model(**inputs)
  print(test, outputs[0])

"""
training_args = TrainingArguments(
    output_dir='./results',          # output directory
    num_train_epochs=3,              # total # of training epochs
    per_device_train_batch_size=16,  # batch size per device during training
    per_device_eval_batch_size=64,   # batch size for evaluation
    warmup_steps=500,                # number of warmup steps for learning rate scheduler
    weight_decay=0.01,               # strength of weight decay
    logging_dir='./logs',            # directory for storing logs
)

trainer = Trainer(
    model=model,                         # the instantiated 🤗 Transformers model to be trained
    args=training_args,                  # training arguments, defined above
    train_dataset=train_dataset,         # training dataset
    eval_dataset=test_dataset            # evaluation dataset
)

trainer.evaluate()
"""
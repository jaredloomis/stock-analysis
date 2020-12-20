struct Strategy {
    rules: Vec<Rule>
}

struct Rule {
    actions: Vec<Action>,
    conditions: Vec<Condition>
}

struct Condition {
    asset: Box<Asset>,

}

enum Action {
    Buy(Box<Asset>),
    Sell(Box<Asset>)
}

trait Asset {
    fn getID(&self) -> &str;
}

use std::collections::VecDeque;
use std::collections::HashMap;
use std::fmt;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

use config::Config;
use layout::Shape;

pub struct FmtEngine<'a> {
    config: Config,
    root: Node<'a>,
}

impl<'a> FmtEngine<'a> {
    pub fn new(text: &'a str) -> FmtEngine<'a> {
        FmtEngine {
            config: Config::default(),
            root: Node { label: None, text: text, kind: NodeKind::List(ListNode::new(RuleSet::empty(), Separator::None)) },
        }
    }

    pub fn reflow(&self, shape: &Shape) -> Result<String, ()> {
        self.reflow_node(&self.root, &[], shape)
    }

    // TODO take inherited effects
    fn reflow_node(&self, node: &Node<'a>, args: &[Arg], shape: &Shape) -> Result<String, ()> {
        match node.kind {
            NodeKind::Separator(sep) => match sep {
                Separator::None => Ok("".to_owned()),
                // TODO should we have a space after the comma?
                Separator::Comma => Ok(",".to_owned()),
                Separator::Space => Ok(" ".to_owned()),
                // TODO break?
                Separator::SpaceOrLineBreak => Ok(" ".to_owned()),
                Separator::LineBreak => Ok("\n".to_owned()),
            },
            NodeKind::Word | NodeKind::Snippet => Ok(node.text.to_owned()),
            NodeKind::List(ref list) => {
                if list.rules.rules.is_empty() {
                    // Rule-free rendering - render each element in turn.
                    //println!("rule free - `{}`", node.text);
                    let mut result = String::new();
                    for element in &list.elements {
                        // TODO compute new shape for each iteration
                        result.push_str(&self.reflow_node(element, &[], shape)?);
                    }
                    return Ok(result);
                }
                for rule in &list.rules.rules {
                    if !self.rule_applies(rule, node, args) {
                        continue;
                    }
                    let result = self.reflow_list_node_with_rule(list, rule, args, shape);
                    if result.is_err() {
                        continue;
                    }
                    //println!("applied {:?}: `{}`", rule, result.as_ref().unwrap());
                    return result;
                }

                // No matching rules.
                Err(())
            }
        }
    }

    fn rule_applies(&self, rule: &Rule, node: &Node, args: &[Arg]) -> bool {
        for condition in &rule.conditions {
            if self.eval_condition(condition, node, args) == 0 {
                return false;
            }
        }
        true
    }

    fn eval_condition(&self, condition: &Condition, node: &Node, args: &[Arg]) -> usize {
        match *condition {
            Condition::Arg(name) => {
                for a in args {
                    if a.0 == name {
                        return a.1;
                    }
                }
                panic!("Arg not present: {}. Found: {:?}", name, args);
            }
            Condition::Query(target, name) => {
                match self.find_real_target(target, node) {
                    Some(target) => {
                        if name == "len" {
                            if let NodeKind::List(ref list) = target.kind {
                                return list.elements.len();
                            } else {
                                panic!("`len` only defined for list nodes, found {:?}", node);
                            }
                        }

                        match self.eval_prop(target, name) {
                            Some(usize) => usize,
                            None => panic!("Property `{}` not found for node {:?}", name, node),
                        }
                    }
                    None => {
                        panic!("Can't find target `{}` from node {:?}", target, node);
                    }
                }
            }
            Condition::Fn(ref c, ref f) => {
                f(self.eval_condition(c, node, args))
            }
        }
    }

    fn eval_prop(&self, node: &Node, prop_name: &'static str) -> Option<usize> {
        match node.kind {
            NodeKind::List(ref list) => {
                for prop in &list.rules.props {
                    if prop_name == prop.name {
                        return Some(self.eval_prop_kind(node, &prop.kind));
                    }
                }
                None                
            }
            NodeKind::Snippet => Some(0),
            NodeKind::Word | NodeKind::Separator(_) => None,
        }
    }

    fn eval_prop_kind(&self, node: &Node, prop: &PropertyKind) -> usize {
        match *prop {
            PropertyKind::Const(c) => c,
            PropertyKind::Fn(ref f) => f(node),
        }
    }

    fn find_real_target(&self, target: &'static str, start_node: &'a Node) -> Option<&'a Node> {
        let mut queue: VecDeque<&'a Node> = VecDeque::new();
        queue.push_back(start_node);

        while !queue.is_empty() {
            let cur_node = queue.pop_front().unwrap();
            if cur_node.matches_target(target) {
                return Some(cur_node);
            }

            match cur_node.kind {
                NodeKind::List(ref list) => {
                    for element in &list.elements {
                        queue.push_back(element);
                    }
                }
                _ => {}
            }
        }

        None
    }

    // Assumes that all the conditions of rule have been met.
    fn reflow_list_node_with_rule(&self, node: &ListNode, rule: &Rule, args: &[Arg], shape: &Shape) -> Result<String, ()> {
        let mut effects = HashMap::new();
        for effect in &rule.effects {
            effects.insert(effect.target, effect.kind.clone());
        }

        let mut result = String::new();
        for element in &node.elements {
            let effect = effects.get(element.text).map(|e| e.clone());
            let effect = effect.or_else(|| element.label.and_then(|l| effects.get(l).map(|e| e.clone())));

            if let Some(EffectKind::PassArg(..)) = effect {
                // TODO modify args
            }

            if let Some(EffectKind::InsertBefore(s)) = effect {
                result.push_str(s);
            }

            // TODO shape
            let node_str = self.reflow_node(element, args, shape)?;
            result.push_str(&node_str);

            if let Some(EffectKind::InsertAfter(s)) = effect {
                result.push_str(s);
            }

            // TODO With effect
        }

        Ok(result)
    }
}

type Arg = (&'static str, usize);

#[derive(Debug)]
pub struct Node<'b> {
    label: Option<&'static str>,
    // TODO should be optional
    text: &'b str,
    kind: NodeKind<'b>,
}

#[derive(Debug)]
enum NodeKind<'b> {
    // TODO rename
    List(ListNode<'b>),
    Separator(Separator),
    Word,
    Snippet,
}

#[derive(Debug)]
struct ListNode<'b> {
    rules: Rc<RuleSet>,
    elements: Vec<Node<'b>>,
    separator: Separator,
}

impl<'b> ListNode<'b> {
    fn new(rules: Rc<RuleSet>, separator: Separator) -> ListNode<'b> {
        ListNode {
            rules: rules,
            elements: vec![],
            separator: separator,
        }
    }
}
#[derive(Copy, Clone, Debug)]
enum Delimiter {
    Brace,
    Paren,
    Bracket,
    Angle,
}

impl Delimiter {
    fn opener(&self) -> &'static str {
        match *self {
            Delimiter::Brace => "{",
            Delimiter::Paren => "(",
            Delimiter::Bracket => "[",
            Delimiter::Angle => "<",
        }
    }

    fn closer(&self) -> &'static str {
        match *self {
            Delimiter::Brace => "}",
            Delimiter::Paren => ")",
            Delimiter::Bracket => "]",
            Delimiter::Angle => ">",
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum Separator {
    None,
    Comma,
    Space,
    SpaceOrLineBreak,
    LineBreak
}

pub struct NodeGuard<'a, 'b: 'a> {
    parent: &'a mut Node<'b>,
    // Should only be None in the dtor.
    node: Option<Box<Node<'b>>>,
}

impl<'b> Node<'b> {
    pub fn item<'a>(&'a mut self, rules: Rc<RuleSet>, text: &'b str) -> NodeGuard<'a, 'b> {
        NodeGuard {
            parent: self,
            node: Some(Box::new(Node { label: None, text: text, kind: NodeKind::List(ListNode::new(rules, Separator::None)) })),
        }
    }

    pub fn list<'a>(&'a mut self, rules: Rc<RuleSet>, sep: Separator, text: &'b str) -> NodeGuard<'a, 'b> {
        NodeGuard {
            parent: self,
            node: Some(Box::new(Node { label: None, text: text, kind: NodeKind::List(ListNode::new(rules, sep)) })),
        }
    }

    pub fn labelled_list<'a>(&'a mut self, label: &'static str, rules: Rc<RuleSet>, sep: Separator, text: &'b str) -> NodeGuard<'a, 'b> {
        NodeGuard {
            parent: self,
            node: Some(Box::new(Node { label: Some(label), text: text, kind: NodeKind::List(ListNode::new(rules, sep)) })),
        }
    }

    pub fn word<'a>(&'a mut self, text: &'b str) {
        if text.is_empty() {
            // TODO maybe warn here
            return;
        }

        match self.kind {
            NodeKind::List(ref mut list) => {
                let node = Node { label: None, text: text, kind: NodeKind::Word };
                list.elements.push(node);
            }
            _ => panic!("Pushing word to atomic node"),
        }
    }

    pub fn space<'a>(&'a mut self, sep: Separator) {
        match self.kind {
            NodeKind::List(ref mut list) => {
                let node = Node { label: None, text: " ", kind: NodeKind::Separator(sep) };
                list.elements.push(node);
            }
            _ => panic!("Pushing spacer to atomic node"),
        }
    }

    pub fn snippet<'a>(&'a mut self, text: &'b str) {
        let node = Node { label: None, text: text, kind: NodeKind::Snippet };
        match self.kind {
            NodeKind::List(ref mut list) => list.elements.push(node),
            _ => panic!("Pushing snippet to atomic node"),
        }
    }

    pub fn pop_node(&mut self, node: Node<'b>) {
        match self.kind {
            NodeKind::List(ref mut list) => list.elements.push(node),
            _ => panic!("pop_node on atomic node"),
        }
    }

    // Non-recursive.
    fn matches_target(&self, target: &'static str) -> bool {
        if target == "self" {
            return true;
        }

        if let Some(label) = self.label {
            if label == target {
                return true;
            }
        }

        match self.kind {
            NodeKind::Word | NodeKind::Snippet => {
                if self.text == target {
                    return true;
                }
            }
            _ => {}
        }

        false
    }
}

impl<'a, 'b> Drop for NodeGuard<'a, 'b> {
    fn drop(&mut self) {
        self.parent.pop_node(*self.node.take().expect("no guarded node?"));
    }
}

impl<'a, 'b> Deref for NodeGuard<'a, 'b> {
    type Target = Node<'b>;
    fn deref(&self) -> &Self::Target {
        self.node.as_ref().unwrap()
    }
}

impl<'a, 'b> DerefMut for NodeGuard<'a, 'b> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.node.as_mut().unwrap()
    }
}

#[derive(Debug)]
struct RuleSet {
    rules: Vec<Rule>,
    props: Vec<Property>,
}

impl RuleSet {
    fn empty() -> Rc<RuleSet> {
        Rc::new(RuleSet {
            rules: vec![],
            props: vec![],
        })
    }
}

#[derive(Debug)]
struct Rule {
    conditions: Vec<Condition>,
    effects: Vec<Effect>,
}

impl Rule {
    fn empty() -> Rule {
        Rule {
            conditions: vec![],
            effects: vec![],
        }
    }
}

enum Condition {
    Arg(&'static str),
    Query(&'static str, &'static str),
    Fn(Box<Condition>, Box<Fn(usize) -> usize>)
}

impl fmt::Debug for Condition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Condition::Arg(ref s) => write!(f, "Arg({})", s),
            Condition::Query(ref t, ref n) => write!(f, "Query({}, {})", t, n),
            Condition::Fn(..) => write!(f, "Fn(..)"),
        }
    }
}

#[derive(Debug)]
struct Effect {
    target: &'static str,
    kind: EffectKind,
}

impl Effect {
    fn pass_arg(target: &'static str, arg: &'static str, value: usize) -> Effect {
        Effect {
            target: target,
            kind: EffectKind::PassArg(arg, value),
        }
    }

    fn insert_after(target: &'static str, value: &'static str) -> Effect {
        Effect {
            target: target,
            kind: EffectKind::InsertAfter(value),
        }
    }

    fn insert_before(target: &'static str, value: &'static str) -> Effect {
        Effect {
            target: target,
            kind: EffectKind::InsertBefore(value),
        }
    }

    fn with(target: &'static str, action: Action) -> Effect {
        Effect {
            target: target,
            kind: EffectKind::With(action),
        }
    }

}

#[derive(Debug, Clone)]
enum EffectKind {
    PassArg(&'static str, usize),
    InsertAfter(&'static str),
    InsertBefore(&'static str),
    With(Action)
}

#[derive(Debug, Copy, Clone)]
enum Action {
    BlockIndent,
    VisualIndent,
    Join(&'static str),
}

#[derive(Debug)]
struct Property {
    name: &'static str,
    kind: PropertyKind,
}

impl Property {
    fn const_(name: &'static str, value: usize) -> Property {
        Property {
            name: name,
            kind: PropertyKind::Const(value),
        }
    }

    fn fn_(name: &'static str, f: Box<Fn(&Node) -> usize>) -> Property {
        Property {
            name: name,
            kind: PropertyKind::Fn(f),
        }        
    }
}

enum PropertyKind {
    Const(usize),
    Fn(Box<Fn(&Node) -> usize>),
}

impl fmt::Debug for PropertyKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            PropertyKind::Const(v) => write!(f, "Const({})", v),
            PropertyKind::Fn(_) => write!(f, "Fn(..)"),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use layout::Shape;
    use indent::Indent;

    #[test]
    fn smoke() {
        let mut engine = FmtEngine::new("fn foo() { bar(); }");
        {
            let mut node = engine.root.item(RuleSet::empty(), "fn foo() { bar(); }");
            node.word("fn");
            node.space(Separator::Space);
            node.word("foo");
            {
                let mut args = node.item(RuleSet::empty(), "()");
                args.word("(");
                args.list(arg_list_rules(), Separator::Comma, "");
                args.word(")");
            }
            node.space(Separator::Space);
            {
                let mut body = node.item(block_rules(), "{ bar(); }");
                body.word("{");
                {
                    let mut stmts = body.labelled_list("stmt_list", stmt_list_rules(), Separator::LineBreak, "bar();");
                    let mut s = stmts.item(RuleSet::empty(), "bar();");
                    s.snippet("bar();");
                }
                body.word("}");
            }
        }
        let result = engine.reflow(&Shape::indented(Indent::new())).unwrap();

        // TODO failing - no spaces - need the lowering step of inserting default spacers
        println!("result: `{}`", result);
        assert!(result == "fn foo() { bar(); }");
    }

    fn block_rules() -> Rc<RuleSet> {
        let mut rules = vec![];

        // Empty block.
        let mut rule = Rule::empty();
        rule.conditions.push(Condition::Fn(Box::new(Condition::Query("stmt_list", "len")), Box::new(|l| if l == 0 { 1 } else { 0 })));
        rules.push(rule);

        // One-line (short) block.
        let mut rule = Rule::empty();
        rule.conditions.push(Condition::Query("stmt_list", "is_short"));
        // TODO this is not the right way to solve this. We should permit influencing the shape for formatting, and have a shape constrained by width + max of single line
        // rule.conditions.push(Condition::Fn(Box::new(Condition::Query("result(stmt_list)", "lines")), Box::new(|l| if l == 1 { 1 } else { 0 })));
        rule.effects.push(Effect::insert_after("{", " "));
        rule.effects.push(Effect::insert_before("}", " "));
        rules.push(rule);

        // Regular block.
        let mut rule = Rule::empty();
        rule.effects.push(Effect::insert_after("{", "\n"));
        rule.effects.push(Effect::with("stmt_list", Action::BlockIndent));
        rule.effects.push(Effect::insert_before("}", "\n"));
        rules.push(rule);

        Rc::new(RuleSet { rules: rules, props: vec![] })
    }

    fn stmt_list_rules() -> Rc<RuleSet> {
        let props = vec![Property::fn_("is_short", Box::new(|node| {
            if let NodeKind::List(ref list) = node.kind {
                if list.elements.len() != 1 {
                    0
                } else {
                    // TODO comments, is_expression
                    1
                }
            } else {
                unreachable!();
            }
        }))];

        Rc::new(RuleSet { rules: vec![], props: props })
    }

    fn arg_list_rules() -> Rc<RuleSet> {
        let mut rules = vec![];

        let mut rule = Rule::empty();
        rule.effects.push(Effect::with("self", Action::Join(", ")));
        rules.push(rule);

        let mut rule = Rule::empty();
        rule.effects.push(Effect::with("self", Action::VisualIndent));
        rule.effects.push(Effect::with("self", Action::Join(",\n")));
        rules.push(rule);

        Rc::new(RuleSet { rules: rules, props: vec![] })
    }
}

use swc_ecma_ast::Decorator;
use swc_ecma_visit::{Node, Visit, VisitWith};

pub(super) fn has_decorator<T: VisitWith<DecoratorFinder>>(node: &T) -> bool {
    let mut v = DecoratorFinder { found: false };
    node.visit_with(&mut v);

    v.found
}

pub(super) struct DecoratorFinder {
    found: bool,
}

impl Visit for DecoratorFinder {
    fn visit_decorator(&mut self, _: &Decorator, _parent: &dyn Node) {
        self.found = true;
    }
}

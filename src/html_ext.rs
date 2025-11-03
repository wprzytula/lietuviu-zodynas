pub(crate) trait NodeExt {
    fn extract_self_or_first_child_as_text(&self) -> Option<&str>;

    fn extract_self_or_first_child_as_text_recursive(&self) -> Option<&str>;
}
impl NodeExt for html_parser::Node {
    fn extract_self_or_first_child_as_text(&self) -> Option<&str> {
        match self {
            html_parser::Node::Element(element) => element.extract_first_text_child(),
            html_parser::Node::Text(text) => Some(text),
            _ => None,
        }
    }

    fn extract_self_or_first_child_as_text_recursive(&self) -> Option<&str> {
        match self {
            html_parser::Node::Element(element) => element.extract_first_text_child_recursive(),
            html_parser::Node::Text(text) => Some(text),
            _ => None,
        }
    }
}

pub(crate) trait ElementExt {
    fn extract_first_text_child(&self) -> Option<&str>;

    fn extract_first_text_child_recursive(&self) -> Option<&str>;

    fn class_contains(&self, class_name: &str) -> bool;
}
impl ElementExt for html_parser::Element {
    fn extract_first_text_child(&self) -> Option<&str> {
        self.children
            .iter()
            .filter_map(|child| child.text())
            .next()
            .map(str::trim)
    }

    fn extract_first_text_child_recursive(&self) -> Option<&str> {
        self.extract_first_text_child().or_else(|| {
            self.children
                .iter()
                .filter_map(|child| child.element())
                .find_map(|element| element.extract_first_text_child_recursive())
        })
    }

    fn class_contains(&self, class_name: &str) -> bool {
        self.classes.iter().any(|class| class == class_name)
    }
}

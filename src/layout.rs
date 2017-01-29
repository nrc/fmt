use indent::Indent;

pub struct Shape {
    indent: Indent,
}

impl Shape {
    pub fn indented(indent: Indent) -> Shape {
        Shape {
            indent: indent,
        }
    }
}

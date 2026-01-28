use std::collections::{BTreeMap, BTreeSet, HashMap};

pub use arktype_ast_macros::ArkType;

pub type SchemaMap = BTreeMap<String, Schema>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Schema {
    pub expr: Expr,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Field {
    pub name: String,
    pub expr: Expr,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Primitive {
    String,
    Boolean,
    Number,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    Primitive(Primitive),
    Ref(String),
    Object(Vec<Field>),
    TypeCall(Box<Expr>),
    Array(Box<Expr>),
    Optional(Box<Expr>),
    Or(Box<Expr>, Vec<Expr>),
    Raw(String),
}

impl Schema {
    pub fn new(expr: Expr) -> Self {
        Self { expr }
    }

    pub fn deps(&self) -> BTreeSet<String> {
        deps_expr(&self.expr)
    }

    pub fn render(
        &self,
        name: &str,
    ) -> String {
        let expr = render_expr(&self.expr);
        format!(
            "const {} = {}\ntype {} = typeof {}.infer\n",
            name, expr, name, name
        )
    }

    pub fn rename(
        &self,
        mapping: &HashMap<String, String>,
    ) -> Self {
        Self {
            expr: rename_expr(&self.expr, mapping),
        }
    }
}

fn deps_expr(expr: &Expr) -> BTreeSet<String> {
    let mut out = BTreeSet::new();
    collect_deps(expr, &mut out);
    out
}

fn collect_deps(
    expr: &Expr,
    out: &mut BTreeSet<String>,
) {
    match expr {
        Expr::Primitive(_) => {},
        Expr::Ref(name) => {
            out.insert(name.clone());
        },
        Expr::Object(fields) => {
            for field in fields {
                collect_deps(&field.expr, out);
            }
        },
        Expr::TypeCall(inner) => collect_deps(inner, out),
        Expr::Array(inner) => collect_deps(inner, out),
        Expr::Optional(inner) => collect_deps(inner, out),
        Expr::Or(base, others) => {
            collect_deps(base, out);
            for expr in others {
                collect_deps(expr, out);
            }
        },
        Expr::Raw(_) => {},
    }
}

fn rename_expr(
    expr: &Expr,
    mapping: &HashMap<String, String>,
) -> Expr {
    match expr {
        Expr::Primitive(p) => Expr::Primitive(*p),
        Expr::Ref(name) => Expr::Ref(rename_name(name, mapping)),
        Expr::Object(fields) => Expr::Object(
            fields
                .iter()
                .map(|field| Field {
                    name: rename_name(&field.name, mapping),
                    expr: rename_expr(&field.expr, mapping),
                })
                .collect(),
        ),
        Expr::TypeCall(inner) => Expr::TypeCall(Box::new(rename_expr(inner, mapping))),
        Expr::Array(inner) => Expr::Array(Box::new(rename_expr(inner, mapping))),
        Expr::Optional(inner) => Expr::Optional(Box::new(rename_expr(inner, mapping))),
        Expr::Or(base, others) => Expr::Or(
            Box::new(rename_expr(base, mapping)),
            others
                .iter()
                .map(|expr| rename_expr(expr, mapping))
                .collect(),
        ),
        Expr::Raw(value) => Expr::Raw(value.clone()),
    }
}

fn rename_name(
    name: &str,
    mapping: &HashMap<String, String>,
) -> String {
    mapping
        .get(name)
        .cloned()
        .unwrap_or_else(|| name.to_string())
}

fn render_expr(expr: &Expr) -> String {
    match expr {
        Expr::Primitive(p) => match p {
            Primitive::String => "type.string".to_string(),
            Primitive::Boolean => "type.boolean".to_string(),
            Primitive::Number => "type.number".to_string(),
        },
        Expr::Ref(name) => name.clone(),
        Expr::Object(fields) => {
            let mut rendered_fields = Vec::with_capacity(fields.len());
            for field in fields {
                let expr = render_expr(&field.expr);
                rendered_fields.push(format!("{}: {}", field.name, expr));
            }
            format!("{{ {} }}", rendered_fields.join(", "))
        },
        Expr::TypeCall(inner) => {
            let inner = render_expr(inner);
            format!("type({inner})")
        },
        Expr::Array(inner) => {
            let inner = render_expr(inner);
            format!("{inner}.array()")
        },
        Expr::Optional(inner) => {
            let inner = render_expr(inner);
            format!("{inner}.or(type.null)")
        },
        Expr::Or(base, others) => {
            let mut rendered = render_expr(base);
            for expr in others {
                let part = render_expr(expr);
                rendered.push_str(&format!(".or({part})"));
            }
            rendered
        },
        Expr::Raw(value) => value.clone(),
    }
}

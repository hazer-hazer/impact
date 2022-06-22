pub enum DefKind {}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct DefId(pub u32);

/**
 * Definition of item, e.g. type
 */
pub struct Def {
    def_id: DefId,
    kind: DefKind,
}

impl Def {
    pub fn new(def_id: DefId, kind: DefKind) -> Self {
        Self { def_id, kind }
    }

    pub fn def_id(&self) -> DefId {
        self.def_id
    }

    pub fn kind(&self) -> &DefKind {
        &self.kind
    }
}

pub enum Namespace {
    Value, // Value namespace used for locals
    Type,  // Type namespace used for types and modules
}

pub struct PerNS<T> {
    value: T,
    ty: T,
}

impl<T> Default for PerNS<T>
where
    T: Default,
{
    fn default() -> Self {
        Self {
            ..Default::default()
        }
    }
}

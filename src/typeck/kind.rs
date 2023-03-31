use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    fmt::Display,
    hash::{Hash, Hasher},
    sync::RwLock,
};

use once_cell::sync::Lazy;

use crate::{
    cli::color::{Color, Colorize},
    dt::idx::{declare_idx, Idx},
};

declare_idx!(KindId, u32, "{}", Color::White);

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Kind(KindId);

impl Kind {
    pub fn intern(data: KindData) -> Self {
        Self(KIND_INTERNER.write().unwrap().intern(data))
    }

    fn new(kind: KindKind) -> Self {
        Self::intern(KindData { kind })
    }

    pub fn ty() -> Self {
        Self::new(KindKind::Ty)
    }

    pub fn cons(domain: Kind, range: Kind) -> Self {
        Self::new(KindKind::Cons(domain, range))
    }

    pub fn kind(&self) -> &KindKind {
        &self.data().kind
    }

    fn data(&self) -> &KindData {
        KIND_INTERNER.read().unwrap().expect(self.0)
    }
}

impl Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind() {
            KindKind::Ty => write!(f, "*"),
            KindKind::Cons(domain, range) => write!(f, "{} -> {}", domain, range),
        }
    }
}

// Yes, KindKind, you see it, it is right here
#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub enum KindKind {
    Ty,
    Cons(Kind, Kind),
}

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub struct KindData {
    kind: KindKind,
}

pub struct KindInterner {
    map: HashMap<u64, KindId>,
    kinds: Vec<&'static KindData>,
}

impl KindInterner {
    pub fn new() -> Self {
        Self {
            map: Default::default(),
            kinds: Default::default(),
        }
    }

    fn hash(kind: &KindData) -> u64 {
        let mut state = DefaultHasher::new();
        kind.hash(&mut state);
        state.finish()
    }

    pub fn intern(&mut self, data: KindData) -> KindId {
        let hash = Self::hash(&data);

        if let Some(id) = self.map.get(&hash) {
            return *id;
        }

        // !Leaked
        let ty = Box::leak(Box::new(data));
        let id = KindId::from(self.kinds.len());

        self.map.insert(hash, id);
        self.kinds.push(ty);

        id
    }

    pub fn expect(&self, id: KindId) -> &'static KindData {
        self.kinds
            .get(id.as_usize())
            .expect(&format!("Failed to find type by type id {}", id))
    }
}

static KIND_INTERNER: Lazy<RwLock<KindInterner>> = Lazy::new(|| RwLock::new(KindInterner::new()));

use crate::chapter10_parser::parser::Type;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum VarKind {
    Static,
    Field,
    Arg,
    Var,
}

#[derive(Debug, PartialEq)]
pub struct Entry {
    pub typ: Type,
    pub kind: VarKind,
    pub index: usize,
}

#[derive(Debug, PartialEq)]
pub struct SymbolTable {
    counts: HashMap<VarKind, usize>,
    table: HashMap<String, Entry>,
}

impl SymbolTable {
    pub fn new() -> Self {
        let mut counts = HashMap::with_capacity(4);
        counts.insert(VarKind::Static, 0);
        counts.insert(VarKind::Field, 0);
        counts.insert(VarKind::Arg, 0);
        counts.insert(VarKind::Var, 0);
        SymbolTable {
            table: HashMap::new(),
            counts,
        }
    }

    pub fn reset(&mut self) {
        for (_, cnt) in self.counts.iter_mut() {
            *cnt = 0
        }
        self.table.clear();
    }

    pub fn define(&mut self, name: impl ToString, typ: Type, kind: VarKind) {
        let name = name.to_string();
        assert!(!self.table.contains_key(&name));

        let cnt = self.counts.get_mut(&kind).unwrap();
        let index = *cnt;
        *cnt += 1;

        self.table.insert(name, Entry { typ, kind, index });
    }

    pub fn count(&self, kind: VarKind) -> usize {
        self.counts[&kind]
    }

    pub fn find(&self, name: &str) -> Option<&Entry> {
        self.table.get(name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resetting_the_symbol_table_is_like_construction() {
        let mut table = SymbolTable::new();
        table.define("foo", Type::Void, VarKind::Static);

        table.reset();

        assert_eq!(table, SymbolTable::new());
    }

    #[test]
    fn define_static() {
        let mut table = SymbolTable::new();
        table.define("foo", Type::Void, VarKind::Static);
        assert_eq!(table.count(VarKind::Static), 1);
        assert_eq!(table.count(VarKind::Field), 0);
        assert_eq!(table.count(VarKind::Arg), 0);
        assert_eq!(table.count(VarKind::Var), 0);
    }

    #[test]
    fn find_in_empty_returns_none() {
        assert_eq!(SymbolTable::new().find("foo"), None);
    }

    #[test]
    fn find_matching_returns_entry() {
        let mut table = SymbolTable::new();
        table.define("foo", Type::Void, VarKind::Static);

        let entry = table.find("foo");
        assert!(entry.is_some());
    }

    #[test]
    fn var_kinds_count_indices_indepentently() {
        let mut table = SymbolTable::new();
        table.define("foo", Type::Void, VarKind::Static);
        table.define("bar", Type::Void, VarKind::Field);
        table.define("baz", Type::Void, VarKind::Static);

        let foo_entry = table.find("foo").unwrap();
        let bar_entry = table.find("bar").unwrap();
        let baz_entry = table.find("baz").unwrap();

        assert_eq!(foo_entry.index, 0);
        assert_eq!(bar_entry.index, 0);
        assert_eq!(baz_entry.index, 1);
    }
}

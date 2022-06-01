use std::collections::HashMap;

pub struct SymbolTable {
    table: HashMap<Box<str>, u16>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            table: HashMap::new(),
        }
    }

    pub fn get_address(&self, symbol: &str) -> Option<u16> {
        self.table.get(symbol).copied()
    }

    pub fn add_entry(&mut self, symbol: &str, address: u16) -> Result<(), String> {
        if let Some(old_addr) = self
            .table
            .insert(symbol.to_owned().into_boxed_str(), address)
        {
            Err(format!(
                "Symbol {} exists at addresses {} and {}.",
                symbol, old_addr, address
            ))
        } else {
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_symbol_table_does_not_contain_arbitrary_symbol() {
        let stab = SymbolTable::new();
        assert_eq!(stab.get_address("foo"), None);
    }

    #[test]
    fn add_entry_to_symbol_table() {
        let mut stab = SymbolTable::new();
        stab.add_entry("foo", 123).unwrap();
        assert_eq!(stab.get_address("foo"), Some(123));
    }

    #[test]
    fn can_add_multiple_symbols() {
        let mut stab = SymbolTable::new();
        stab.add_entry("foo", 12).unwrap();
        stab.add_entry("bar", 34).unwrap();
        assert_eq!(stab.get_address("foo"), Some(12));
        assert_eq!(stab.get_address("bar"), Some(34));
    }

    #[test]
    fn cant_add_symbols_twice() {
        let mut stab = SymbolTable::new();
        stab.add_entry("foo", 12).unwrap();
        assert!(stab.add_entry("foo", 34).is_err());
    }
}

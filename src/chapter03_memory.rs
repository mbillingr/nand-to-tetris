use crate::hardware::{SystemBuilder, Wire};
use crate::chapter01_boolean_logic::Bit;

pub fn make_register(
    sb: &mut SystemBuilder<Bit>,
    name: impl Into<String>,
    i: &Wire<Bit>,
    load: &Wire<Bit>,
    o: &Wire<Bit>,
) {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hardware::{BusApi, ClockHandler};
    use Bit::{I, O};

    #[test]
    fn test_register() {
        system! {
            sys
            wires { i, load, o }
            gates {
                make_register("REG", i, load, o);
            }
            body {
                assert_sim!(sys, i=O, load=O =>);
                sys.cycle();
                assert_sim!(sys, i=O, load=O => o=O);
                sys.cycle();
                assert_sim!(sys, i=I, load=O => o=O);
                sys.cycle();
                assert_sim!(sys, i=O, load=I => o=O);
                sys.cycle();
                assert_sim!(sys, i=I, load=I => o=I);
                sys.cycle();
                assert_sim!(sys, i=O, load=O => o=I);
                sys.cycle();
                assert_sim!(sys, i=I, load=O => o=I);
                sys.cycle();
                assert_sim!(sys, i=I, load=I => o=I);
                sys.cycle();
                assert_sim!(sys, i=O, load=I => o=O);
            }
        }
    }
}
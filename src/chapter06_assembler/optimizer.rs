use crate::chapter06_assembler::parser::{Comp, Dest, Instruction, Jump, Parser};

use Instruction::*;

trait PeepholeRule<'s> {
    fn pattern_length(&self) -> usize;
    fn apply(&self, ops: &[Instruction<'s>]) -> Option<Vec<Instruction<'s>>>;
}

pub struct PeepholeOptimizer<'s> {
    rules: Vec<Box<dyn PeepholeRule<'s>>>,
}

impl<'s> Default for PeepholeOptimizer<'s> {
    fn default() -> Self {
        PeepholeOptimizer {
            rules: vec![
                Box::new(IndirectAssignment),
                Box::new(TrivialConstant4),
                Box::new(TrivialConstant5),
                Box::new(RedundantInstruction),
                Box::new(UselessPairs),
                Box::new(DegenerateStackoperation),
                Box::new(RemoveNops),
            ],
        }
    }
}

impl<'s> PeepholeOptimizer<'s> {
    pub fn optimize(&self, ops: impl IntoIterator<Item = Instruction<'s>>) -> Vec<Instruction<'s>> {
        let mut result = vec![];
        for op in ops {
            result.push(op);
            let mut converged = false;
            while !converged {
                converged = true;
                for rule in &self.rules {
                    if result.len() < rule.pattern_length() {
                        continue;
                    }

                    let pattern_start_index = result.len() - rule.pattern_length();

                    if let Some(new_ops) = rule.apply(&result[pattern_start_index..]) {
                        result.truncate(pattern_start_index);
                        result.extend(new_ops);
                        converged = false;
                    }
                }
            }
        }
        result
    }

    pub fn optimize_str(&self, asm_code: &'s str) -> Result<String, String> {
        let mut ops = vec![];
        let mut parser = Parser::new(asm_code);
        while parser.has_more_lines() {
            parser.advance();
            ops.push(parser.instruction()?);
        }

        let opt = self.optimize(ops);

        let asm_lines: Vec<_> = opt.into_iter().map(|op| op.to_string()).collect();
        Ok(asm_lines.join("\n"))
    }
}

struct IndirectAssignment;

impl<'s> PeepholeRule<'s> for IndirectAssignment {
    fn pattern_length(&self) -> usize {
        3
    }

    fn apply(&self, ops: &[Instruction<'s>]) -> Option<Vec<Instruction<'s>>> {
        match ops {
            [C(Dest::A, comp, Jump::None), C(Dest::D, Comp::A, Jump::None), next @ (A(_) | C(Dest::A, _, _))] => {
                Some(vec![C(Dest::D, *comp, Jump::None), *next])
            }
            _ => None,
        }
    }
}

struct TrivialConstant4;

impl<'s> PeepholeRule<'s> for TrivialConstant4 {
    fn pattern_length(&self) -> usize {
        4
    }

    fn apply(&self, ops: &[Instruction<'s>]) -> Option<Vec<Instruction<'s>>> {
        match ops {
            [A(c @ ("0" | "1" | "-1")), C(Dest::D, Comp::A, Jump::None), A(ptr), C(dest, Comp::D, jmp)] =>
            {
                let comp = match *c {
                    "0" => Comp::Zero,
                    "1" => Comp::One,
                    "-1" => Comp::MinusOne,
                    _ => unreachable!(),
                };
                Some(vec![A(ptr), C(*dest, comp, *jmp)])
            }
            _ => None,
        }
    }
}

struct TrivialConstant5;

impl<'s> PeepholeRule<'s> for TrivialConstant5 {
    fn pattern_length(&self) -> usize {
        5
    }

    fn apply(&self, ops: &[Instruction<'s>]) -> Option<Vec<Instruction<'s>>> {
        match ops {
            [A(c @ ("0" | "1" | "-1")), C(Dest::D, Comp::A, Jump::None), A(ptr), middle @ C(
                Dest::A | Dest::M | Dest::AM,
                Comp::M
                | Comp::A
                | Comp::IncM
                | Comp::IncA
                | Comp::DecM
                | Comp::DecA
                | Comp::NegA
                | Comp::NegM
                | Comp::NotA
                | Comp::NotM
                | Comp::One
                | Comp::Zero
                | Comp::MinusOne,
                Jump::None,
            ), C(dest, Comp::D, jmp)] => {
                let comp = match *c {
                    "0" => Comp::Zero,
                    "1" => Comp::One,
                    "-1" => Comp::MinusOne,
                    _ => unreachable!(),
                };
                Some(vec![A(ptr), *middle, C(*dest, comp, *jmp)])
            }
            _ => None,
        }
    }
}

struct RedundantInstruction;

impl<'s> PeepholeRule<'s> for RedundantInstruction {
    fn pattern_length(&self) -> usize {
        3
    }

    fn apply(&self, ops: &[Instruction<'s>]) -> Option<Vec<Instruction<'s>>> {
        match ops {
            [A(x), op @ C(Dest::D | Dest::M | Dest::DM, _, Jump::None), A(y)] if x == y => {
                Some(vec![A(x), *op])
            }
            _ => None,
        }
    }
}

struct UselessPairs;

impl<'s> PeepholeRule<'s> for UselessPairs {
    fn pattern_length(&self) -> usize {
        2
    }

    fn apply(&self, ops: &[Instruction<'s>]) -> Option<Vec<Instruction<'s>>> {
        match ops {
            [C(Dest::A, Comp::IncA, Jump::None), C(Dest::A, Comp::DecA, Jump::None)]
            | [C(Dest::A, Comp::DecA, Jump::None), C(Dest::A, Comp::IncA, Jump::None)]
            | [C(Dest::A, Comp::NegA, Jump::None), C(Dest::A, Comp::NegA, Jump::None)]
            | [C(Dest::A, Comp::NotA, Jump::None), C(Dest::A, Comp::NotA, Jump::None)]
            | [C(Dest::A, Comp::AminusD, Jump::None), C(Dest::A, Comp::DplusA, Jump::None)]
            | [C(Dest::A, Comp::DplusA, Jump::None), C(Dest::A, Comp::AminusD, Jump::None)]
            | [C(Dest::D, Comp::IncD, Jump::None), C(Dest::D, Comp::DecD, Jump::None)]
            | [C(Dest::D, Comp::DecD, Jump::None), C(Dest::D, Comp::IncD, Jump::None)]
            | [C(Dest::D, Comp::NegD, Jump::None), C(Dest::D, Comp::NegD, Jump::None)]
            | [C(Dest::D, Comp::NotD, Jump::None), C(Dest::D, Comp::NotD, Jump::None)]
            | [C(Dest::D, Comp::DminusA, Jump::None), C(Dest::D, Comp::DplusA, Jump::None)]
            | [C(Dest::D, Comp::DplusA, Jump::None), C(Dest::D, Comp::DminusA, Jump::None)]
            | [C(Dest::M, Comp::IncM, Jump::None), C(Dest::M, Comp::DecM, Jump::None)]
            | [C(Dest::M, Comp::DecM, Jump::None), C(Dest::M, Comp::IncM, Jump::None)]
            | [C(Dest::M, Comp::NegM, Jump::None), C(Dest::M, Comp::NegM, Jump::None)]
            | [C(Dest::M, Comp::NotM, Jump::None), C(Dest::M, Comp::NotM, Jump::None)]
            | [C(Dest::M, Comp::MminusD, Jump::None), C(Dest::M, Comp::DplusM, Jump::None)]
            | [C(Dest::M, Comp::DplusM, Jump::None), C(Dest::M, Comp::MminusD, Jump::None)] => {
                Some(vec![])
            }
            _ => None,
        }
    }
}

struct DegenerateStackoperation;

impl<'s> PeepholeRule<'s> for DegenerateStackoperation {
    fn pattern_length(&self) -> usize {
        6
    }

    fn apply(&self, ops: &[Instruction<'s>]) -> Option<Vec<Instruction<'s>>> {
        match ops {
            // this simplification is only allowed on the stack, otherwise the side-effect of writing the memory location would need to be preserved.
            [A("SP"), C(Dest::A, Comp::M, Jump::None), C(Dest::M, comp, Jump::None), A("SP"), C(Dest::A, Comp::M, Jump::None), C(dest, Comp::M, jmp)] => {
                Some(vec![C(*dest, *comp, *jmp)])
            }
            _ => None,
        }
    }
}

struct RemoveNops;

impl<'s> PeepholeRule<'s> for RemoveNops {
    fn pattern_length(&self) -> usize {
        1
    }

    fn apply(&self, ops: &[Instruction<'s>]) -> Option<Vec<Instruction<'s>>> {
        match ops {
            [C(Dest::None, _, Jump::None)]
            | [C(Dest::A, Comp::A, Jump::None)]
            | [C(Dest::D, Comp::D, Jump::None)]
            | [C(Dest::M, Comp::M, Jump::None)] => Some(vec![]),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_opt {
        ($code:expr, $expect:expr) => {{
            let opt = PeepholeOptimizer::default();
            let result = opt.optimize($code);
            assert_eq!(result, $expect);
        }};
    }

    #[test]
    fn indirect_assignment() {
        assert_opt!(
            [
                C(Dest::A, Comp::M, Jump::None),
                C(Dest::D, Comp::A, Jump::None),
                A("ARG"),
            ],
            [C(Dest::D, Comp::M, Jump::None), A("ARG"),]
        );
    }

    #[test]
    fn trivial_constant4() {
        assert_opt!(
            [
                A("0"),
                C(Dest::D, Comp::A, Jump::None),
                A("ARG"),
                C(Dest::M, Comp::D, Jump::None),
            ],
            [A("ARG"), C(Dest::M, Comp::Zero, Jump::None),]
        );
    }

    #[test]
    fn trivial_constant5() {
        assert_opt!(
            [
                A("0"),
                C(Dest::D, Comp::A, Jump::None),
                A("ARG"),
                C(Dest::A, Comp::IncM, Jump::None),
                C(Dest::M, Comp::D, Jump::None),
            ],
            [
                A("ARG"),
                C(Dest::A, Comp::IncM, Jump::None),
                C(Dest::M, Comp::Zero, Jump::None),
            ]
        );
    }

    #[test]
    fn redundant_instruction() {
        assert_opt!(
            [A("FOO"), C(Dest::M, Comp::A, Jump::None), A("FOO")],
            [A("FOO"), C(Dest::M, Comp::A, Jump::None)]
        );
    }

    #[test]
    fn useless_pairs() {
        assert_opt!(
            [
                C(Dest::M, Comp::IncM, Jump::None),
                C(Dest::M, Comp::DecM, Jump::None)
            ],
            []
        );
        assert_opt!(
            [
                C(Dest::M, Comp::DecM, Jump::None),
                C(Dest::M, Comp::IncM, Jump::None)
            ],
            []
        );
        assert_opt!(
            [
                C(Dest::D, Comp::NotD, Jump::None),
                C(Dest::D, Comp::NotD, Jump::None)
            ],
            []
        );
    }

    #[test]
    fn stack_nop() {
        assert_opt!(
            [
                A("SP"),
                C(Dest::A, Comp::M, Jump::None),
                C(Dest::M, Comp::One, Jump::None),
                A("SP"),
                C(Dest::A, Comp::M, Jump::None),
                C(Dest::D, Comp::M, Jump::None),
            ],
            [C(Dest::D, Comp::One, Jump::None)]
        );
    }

    #[test]
    fn remove_nops() {
        assert_opt!([C(Dest::None, Comp::A, Jump::None)], []);
        assert_opt!([C(Dest::A, Comp::A, Jump::None)], []);
        assert_opt!([C(Dest::D, Comp::D, Jump::None)], []);
        assert_opt!([C(Dest::M, Comp::M, Jump::None)], []);
    }
}

pub mod scanner;

use std::{env, io::{stdin, Stdin}, process::exit};

type Value = f64;

#[derive(Clone, Copy, Debug)]
enum Instr {
    OpNegate,
    OpConstant(Value),
    OpReturn,
    OpBinAdd,
    OpBinSub,
    OpBinDiv,
    OpBinMult,
}

enum InterpretError {
    CompilerError,
    RuntimeError
}

struct VirtualMachine <'a> {
    code : &'a Vec<Instr>,
    stack : &'a mut Vec<Value>,
    ip : usize, // index of code vec
}

fn exec(vm : &mut VirtualMachine) {
    loop {
        let instr = vm.code[vm.ip];
        vm.ip += 1; 
        match instr {
            Instr::OpConstant(val) => {
                vm.stack.push(val);
            },
            Instr::OpReturn => {
                if let Some(val) = vm.stack.pop() {
                    println!("{}", val);
                } else {
                    panic!("Could not pop value for {:?}", instr);
                }
            },
            Instr::OpNegate => {
                if let Some(val) = vm.stack.pop() {
                    vm.stack.push(-val);
                } else {
                    panic!("Could not pop value for {:?}", instr);
                }                
            },
            Instr::OpBinAdd => {
                let first_opt = vm.stack.pop();
                let sec_opt = vm.stack.pop();
                match (first_opt, sec_opt) {
                    (Some(lhs), Some(rhs)) => vm.stack.push(lhs + rhs),
                    (None, None) | (Some(_), None) | (None, Some(_)) => panic!("Could not pop value for {:?}", instr)
                }
            },
            Instr::OpBinDiv => {
                let first_opt = vm.stack.pop();
                let sec_opt = vm.stack.pop();
                match (first_opt, sec_opt) {
                    (Some(lhs), Some(rhs)) => vm.stack.push(lhs / rhs),
                    (None, None) | (Some(_), None) | (None, Some(_)) => panic!("Could not pop value for {:?}", instr)
                }
            },
            Instr::OpBinSub => {
                let first_opt = vm.stack.pop();
                let sec_opt = vm.stack.pop();
                match (first_opt, sec_opt) {
                    (Some(lhs), Some(rhs)) => vm.stack.push(lhs - rhs),
                    (None, None) | (Some(_), None) | (None, Some(_)) => panic!("Could not pop value for {:?}", instr)
                }
            },
            Instr::OpBinMult => {
                let first_opt = vm.stack.pop();
                let sec_opt = vm.stack.pop();
                match (first_opt, sec_opt) {
                    (Some(lhs), Some(rhs)) => vm.stack.push(lhs * rhs),
                    (None, None) | (Some(_), None) | (None, Some(_)) => panic!("Could not pop value for {:?}", instr)
                }
            },
        }
    }
}

fn repl() {
    let mut buf = String::new();
    let res = std::io::stdin().read_line(&mut buf);
    match res {
        Ok(0) => exit(0),
        Ok(_) => todo!(),
        Err(foo) => eprintln!("{}", foo)
    }
}

fn main() {
    let args : Vec<String> = env::args().collect();
    if args.len() == 1 {
        repl()
    } else if args.len() == 2 {
        // TODO: run files
    } else {
        eprintln!("usage: rlox [path]");
        exit(127);
    }
}

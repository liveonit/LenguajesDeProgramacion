use std::collections::HashMap;
use std::io::{self, BufRead};
use std::*;
use std::process::exit;


fn main() {
    let mut operations: HashMap<&str, fn(Vec<f64>, [f64; 10], usize) -> (Vec<f64>, [f64; 10])> = HashMap::new();
    operations.insert("ADD", add);
    operations.insert("SUB", sub);
    operations.insert("MULT", mult);
    operations.insert("DIV", div);
    operations.insert("EQ", eq);
    operations.insert("DIFF", diff);
    operations.insert("LT", lt);
    operations.insert("LTE", lte);
    operations.insert("GT", gt);
    operations.insert("GTE", gte);
    operations.insert("AND", and);
    operations.insert("OR", or);
    operations.insert("NOT", not);
    operations.insert("DUP", dup);
    operations.insert("POP", pop);
    operations.insert("GET", get);
    operations.insert("SET", set);

    stackalc(operations);
}


fn stackalc(operations: HashMap<&str, fn(Vec<f64>, [f64; 10], usize) -> (Vec<f64>, [f64; 10])>) {
    let mut stack : Vec<f64> = Vec::new();
    let mut variables: [f64; 10] = [f64::NAN; 10];
    let mut lines = io::stdin().lock().lines();
    while let Some(line) = lines.next() {
        let last_input = line.unwrap();
        // stop storing the user input
        if last_input.len() == 0 {
          println!("Stack: {:?}", stack);
          println!("Variables {:?}", variables);
          exit(0);
        } else {
            for val in last_input.split_whitespace() {
               let inputs = val.split(':').collect::<Vec<&str>>();
                if operations.contains_key(inputs[0]) {
                    let op = operations[inputs[0]];
                    let n = if inputs.len() > 1 {  inputs[1].parse().unwrap()} else { usize::MIN };
                    (stack, variables) = op(stack, variables, n);
                } else {
                  let n:f64 = val.parse().unwrap();
                  stack.push(n);
                }
              }
            println!("Variables {:?}", variables);
            println!("Stack {:?}", stack);
        }
    }
}


fn add(mut stack : Vec<f64>, variables: [f64; 10], _n: usize) -> (Vec<f64>, [f64; 10]) {
    if stack.len() > 1 {
        let item1 : f64 = match stack.pop() {
            Some(f64) => f64,
            None => 0.0
        };
        let item2 : f64 = match stack.pop() {
            Some(f64) => f64,
            None => 0.0
        };
        let result = item1 + item2;
        stack.push(result)
    }
    return (stack, variables);

}

fn mult(mut stack : Vec<f64>, variables: [f64; 10], _n: usize) -> (Vec<f64>, [f64; 10]) {
    if stack.len() > 1 {
        let item1 : f64 = match stack.pop() {
            Some(f64) => f64,
            None => 1.0
        };
        let item2 : f64 = match stack.pop() {
            Some(f64) => f64,
            None => 1.0
        };
        let result = item1 * item2;
        stack.push(result)
    }
    return (stack, variables);

}

fn div(mut stack : Vec<f64>, variables: [f64; 10], _n: usize) -> (Vec<f64>, [f64; 10]) {
    if stack.len() > 1 {
        let item1 : f64 = match stack.pop() {
            Some(f64) => f64,
            None => 1.0
        };
        let item2 : f64 = match stack.pop() {
            Some(f64) => f64,
            None => 1.0
        };
        let result =  item2 / item1;
        stack.push(result)
    }
    return (stack, variables);
}

fn sub(mut stack : Vec<f64>, variables: [f64; 10], _n: usize) -> (Vec<f64>, [f64; 10]) {
    if stack.len() > 1 {
        let item1 : f64 = match stack.pop() {
            Some(f64) => f64,
            None => 0.0
        };
        let item2 : f64 = match stack.pop() {
            Some(f64) => f64,
            None => 0.0
        };
        let result = item2 - item1;
        stack.push(result)
    }
    return (stack, variables);
}

fn eq(mut stack : Vec<f64>, variables: [f64; 10], _n: usize) -> (Vec<f64>, [f64; 10]) {
    if stack.len() > 1 {
        let item1 : f64 = match stack.pop() {
            Some(f64) => f64,
            None => 0.0
        };
        let item2 : f64 = match stack.pop() {
            Some(f64) => f64,
            None => 0.0
        };
        if item1 == item2 {
            stack.push(1.0);
        }
        else {
            stack.push(0.0);
        }
    }
    return (stack, variables);
}

fn diff(mut stack : Vec<f64>, variables: [f64; 10], _n: usize) -> (Vec<f64>, [f64; 10]) {
    if stack.len() > 1 {
        let item1 : f64 = match stack.pop() {
            Some(f64) => f64,
            None => 0.0
        };
        let item2 : f64 = match stack.pop() {
            Some(f64) => f64,
            None => 0.0
        };
        if item1 != item2 {
            stack.push(1.0);
        }
        else {
            stack.push(0.0);
        }
    }
    return (stack, variables);
}

fn lt(mut stack : Vec<f64>, variables: [f64; 10], _n: usize) -> (Vec<f64>, [f64; 10]) {
    if stack.len() > 1 {
        let item1 : f64 = match stack.pop() {
            Some(f64) => f64,
            None => 0.0
        };
        let item2 : f64 = match stack.pop() {
            Some(f64) => f64,
            None => 0.0
        };
        if item1 > item2 {
            stack.push(1.0);
        }
        else {
            stack.push(0.0);
        }
    }
    return (stack, variables);
}

fn lte(mut stack : Vec<f64>, variables: [f64; 10], _n: usize) -> (Vec<f64>, [f64; 10]) {
    if stack.len() > 1 {
        let item1 : f64 = match stack.pop() {
            Some(f64) => f64,
            None => 0.0
        };
        let item2 : f64 = match stack.pop() {
            Some(f64) => f64,
            None => 0.0
        };
        if item1 >= item2 {
            stack.push(1.0);
        }
        else {
            stack.push(0.0);
        }
    }
    return (stack, variables);
}

fn gt(mut stack : Vec<f64>, variables: [f64; 10], _n: usize) -> (Vec<f64>, [f64; 10]) {
    if stack.len() > 1 {
        let item1 : f64 = match stack.pop() {
            Some(f64) => f64,
            None => 0.0
        };
        let item2 : f64 = match stack.pop() {
            Some(f64) => f64,
            None => 0.0
        };
        if item1 < item2 {
            stack.push(1.0);
        }
        else {
            stack.push(0.0);
        }
    }
    return (stack, variables);
}

fn gte(mut stack : Vec<f64>, variables: [f64; 10], _n: usize) -> (Vec<f64>, [f64; 10]) {
    if stack.len() > 1 {
        let item1 : f64 = match stack.pop() {
            Some(f64) => f64,
            None => 0.0
        };
        let item2 : f64 = match stack.pop() {
            Some(f64) => f64,
            None => 0.0
        };
        if item1 <= item2 {
            stack.push(1.0);
        }
        else {
            stack.push(0.0);
        }
    }
    return (stack, variables);
}

fn and(mut stack : Vec<f64>, variables: [f64; 10], _n: usize) -> (Vec<f64>, [f64; 10]) {
    if stack.len() > 1 {
        let item1 : f64 = match stack.pop() {
            Some(f64) => f64,
            None => 0.0
        };
        let item2 : f64 = match stack.pop() {
            Some(f64) => f64,
            None => 0.0
        };
        let result = if item1!=0.0 && !item1.is_nan()  {item1} else {item2};
        stack.push(result)
    }
    return (stack, variables);
}

fn or(mut stack : Vec<f64>, variables: [f64; 10], _n: usize) -> (Vec<f64>, [f64; 10]) {
    if stack.len() > 1 {
        let item1 : f64 = match stack.pop() {
            Some(f64) => f64,
            None => 0.0
        };
        let item2 : f64 = match stack.pop() {
            Some(f64) => f64,
            None => 0.0
        };
        let result = if item1!=0.0 && !item1.is_nan()  {item2} else {item1};
        stack.push(result)
    }
    return (stack, variables);
}

fn not(mut stack : Vec<f64>, variables: [f64; 10], _n: usize) -> (Vec<f64>, [f64; 10]) {
    if stack.len() > 0 {
        let item1 : f64 = match stack.pop() {
            Some(f64) => f64,
            None => 0.0
        };
        let result: f64 = if item1==0.0 {1.0} else {0.0};
        stack.push(result)
    }
    return (stack, variables);
}

fn dup(mut stack : Vec<f64>, variables: [f64; 10], _n: usize) -> (Vec<f64>, [f64; 10]) {
    let item : f64 = stack.pop().unwrap();
    stack.push(item);
    stack.push(item);
    return (stack, variables);
}

fn pop(mut stack : Vec<f64>, variables: [f64; 10], _n: usize) -> (Vec<f64>, [f64; 10]) {
    stack.pop();
    return (stack, variables);
}

fn get(mut stack : Vec<f64>, variables: [f64; 10], n: usize) -> (Vec<f64>, [f64; 10]) {
    stack.push(variables[n]);
    return (stack, variables);
}
fn set(mut stack : Vec<f64>, mut variables: [f64; 10], n: usize) -> (Vec<f64>, [f64; 10]) {
    let item : f64 = stack.pop().unwrap();
    variables[n] = item;
    return (stack, variables);
}

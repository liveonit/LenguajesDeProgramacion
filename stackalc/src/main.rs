use std::collections::HashMap;
use std::io::{self, BufRead};
use std::*;
use std::process::exit;


fn main() {
    let mut operations: HashMap<&str, fn(Vec<f64>) -> Vec<f64>> = HashMap::new();
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

    stackalc(operations);
}


fn stackalc(operations: HashMap<&str, fn(Vec<f64>) -> Vec<f64>>) {
    let mut stack : Vec<f64> = Vec::new();
    let mut lines = io::stdin().lock().lines();
    while let Some(line) = lines.next() {
        let last_input = line.unwrap();
        // stop storing the user input
        if last_input.len() == 0 {
          println!("{:?}", stack);
          exit(0);
        } else {
            for val in last_input.split_whitespace() {
                if operations.contains_key(val) {
                    let op = operations[val];
                    stack = op(stack);
                } else {
                  let n:f64 = val.parse().unwrap();
                  stack.push(n);
                }
            }
        }
    }
}


fn add(mut stack : Vec<f64>) -> Vec<f64> {
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
    return stack;

}

fn mult(mut stack : Vec<f64>) -> Vec<f64> {
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
    return stack;

}

fn div(mut stack : Vec<f64>) -> Vec<f64> {
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
    return stack;

}

fn sub(mut stack : Vec<f64>) -> Vec<f64> {
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
    return stack;

}

fn eq(mut stack : Vec<f64>) -> Vec<f64> {
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
    return stack;
}

fn diff(mut stack : Vec<f64>) -> Vec<f64> {
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
    return stack;
}

fn lt(mut stack : Vec<f64>) -> Vec<f64> {
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
    return stack;
}

fn lte(mut stack : Vec<f64>) -> Vec<f64> {
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
    return stack;
}

fn gt(mut stack : Vec<f64>) -> Vec<f64> {
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
    return stack;
}

fn gte(mut stack : Vec<f64>) -> Vec<f64> {
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
    return stack;
}

fn and(mut stack : Vec<f64>) -> Vec<f64> {
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
    return stack;
}

fn or(mut stack : Vec<f64>) -> Vec<f64> {
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
    return stack;
}

fn not(mut stack : Vec<f64>) -> Vec<f64> {
    if stack.len() > 1 {
        let item1 : f64 = match stack.pop() {
            Some(f64) => f64,
            None => 0.0
        };
        let result: f64 = if item1==0.0 && item1.is_nan()  {0.0} else {1.0};
        stack.push(result)
    }
    return stack;
}

fn dup(mut stack : Vec<f64>) -> Vec<f64> {
    let item : f64 = stack.pop().unwrap();
    stack.push(item);
    stack.push(item);
    return stack;
}

fn pop(mut stack : Vec<f64>) -> Vec<f64> {
    stack.pop();
    return stack;
}

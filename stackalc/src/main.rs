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
    operations.insert("RET", ret);

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
    if (stack.len() > 1) {
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
    if (stack.len() > 1) {
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
    if (stack.len() > 1) {
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
    if (stack.len() > 1) {
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

fn ret(mut stack : Vec<f64>) -> Vec<f64> {
    return stack;
}

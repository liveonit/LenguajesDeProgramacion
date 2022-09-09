use std::collections::HashMap;
use std::io::{self, BufRead};
use std::*;
use std::process::exit;

fn main() {
    let mut operations: HashMap<&str, fn(Vec<f64>, [f64; 10], i32, usize) -> (Vec<f64>, [f64; 10], usize)> = HashMap::new();
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
    operations.insert("UJP", ujp);
    operations.insert("CJP", cjp);

    stackalc(operations);
}


fn stackalc(operations: HashMap<&str, fn(Vec<f64>, [f64; 10], i32, usize) -> (Vec<f64>, [f64; 10], usize)>) {
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
            let input = last_input.split_whitespace().collect::<Vec<&str>>();
            let mut counter: usize = 0;
            while counter < input.len()  {
                let inputs = input[counter].split(':').collect::<Vec<&str>>();
                if operations.contains_key(inputs[0]) {
                    let op = operations[inputs[0]];
                    let n = if inputs.len() > 1 {  inputs[1].parse().unwrap()} else { i32::MIN };
                    (stack, variables, counter) = op(stack, variables, n, counter);
                } else {
                  let n:f64 = input[counter].parse().unwrap();
                  stack.push(n);
                  counter = counter + 1;
                }
            }
        }
    }
    println!("Stack: {:?}", stack);
    println!("Variables {:?}", variables);

}


fn add(mut stack : Vec<f64>, variables: [f64; 10], _n: i32, counter: usize) -> (Vec<f64>, [f64; 10], usize) {
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
    return (stack, variables, counter + 1);
}

fn mult(mut stack : Vec<f64>, variables: [f64; 10], _n: i32, counter: usize) -> (Vec<f64>, [f64; 10], usize) {
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
    return (stack, variables, counter + 1);

}

fn div(mut stack : Vec<f64>, variables: [f64; 10], _n: i32, counter: usize) -> (Vec<f64>, [f64; 10], usize) {
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
    return (stack, variables, counter + 1);
}

fn sub(mut stack : Vec<f64>, variables: [f64; 10], _n: i32, counter: usize) -> (Vec<f64>, [f64; 10], usize) {
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
    return (stack, variables, counter + 1);
}

fn eq(mut stack : Vec<f64>, variables: [f64; 10], _n: i32, counter: usize) -> (Vec<f64>, [f64; 10], usize) {
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
    return (stack, variables, counter + 1);
}

fn diff(mut stack : Vec<f64>, variables: [f64; 10], _n: i32, counter: usize) -> (Vec<f64>, [f64; 10], usize) {
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
    return (stack, variables, counter + 1);
}

fn lt(mut stack : Vec<f64>, variables: [f64; 10], _n: i32, counter: usize) -> (Vec<f64>, [f64; 10], usize) {
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
    return (stack, variables, counter + 1);
}

fn lte(mut stack : Vec<f64>, variables: [f64; 10], _n: i32, counter: usize) -> (Vec<f64>, [f64; 10], usize) {
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
    return (stack, variables, counter + 1);
}

fn gt(mut stack : Vec<f64>, variables: [f64; 10], _n: i32, counter: usize) -> (Vec<f64>, [f64; 10], usize) {
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
    return (stack, variables, counter + 1);
}

fn gte(mut stack : Vec<f64>, variables: [f64; 10], _n: i32, counter: usize) -> (Vec<f64>, [f64; 10], usize) {
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
    return (stack, variables, counter + 1);
}

fn and(mut stack : Vec<f64>, variables: [f64; 10], _n: i32, counter: usize) -> (Vec<f64>, [f64; 10], usize) {
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
    return (stack, variables, counter + 1);
}

fn or(mut stack : Vec<f64>, variables: [f64; 10], _n: i32, counter: usize) -> (Vec<f64>, [f64; 10], usize) {
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
    return (stack, variables, counter + 1);
}

fn not(mut stack : Vec<f64>, variables: [f64; 10], _n: i32, counter: usize) -> (Vec<f64>, [f64; 10], usize) {
    if stack.len() > 0 {
        let item1 : f64 = match stack.pop() {
            Some(f64) => f64,
            None => 0.0
        };
        let result: f64 = if item1==0.0 {1.0} else {0.0};
        stack.push(result)
    }
    return (stack, variables, counter + 1);
}

fn dup(mut stack : Vec<f64>, variables: [f64; 10], _n: i32, counter: usize) -> (Vec<f64>, [f64; 10], usize) {
    let item : f64 = stack.pop().unwrap();
    stack.push(item);
    stack.push(item);
    return (stack, variables, counter + 1);
}

fn pop(mut stack : Vec<f64>, variables: [f64; 10], _n: i32, counter: usize) -> (Vec<f64>, [f64; 10], usize) {
    stack.pop();
    return (stack, variables, counter + 1);
}

fn get(mut stack : Vec<f64>, variables: [f64; 10], n: i32, counter: usize) -> (Vec<f64>, [f64; 10], usize) {
    stack.push(variables[n as usize]);
    return (stack, variables, counter + 1);
}
fn set(mut stack : Vec<f64>, mut variables: [f64; 10], n: i32, counter: usize) -> (Vec<f64>, [f64; 10], usize) {
    let item : f64 = stack.pop().unwrap();
    variables[n as usize] = item;
    return (stack, variables, counter + 1);
}

fn ujp(stack : Vec<f64>, variables: [f64; 10], n: i32, counter: usize) -> (Vec<f64>, [f64; 10], usize) {
  if n < 0 {
    return (stack, variables, counter - ((n * -1) as usize));
  }
  return (stack, variables, counter + n as usize);
}

fn cjp(mut stack : Vec<f64>, variables: [f64; 10], n: i32, counter: usize) -> (Vec<f64>, [f64; 10], usize) {
    let item : f64 = stack.pop().unwrap();
    if item > 0.0 {
        if n < 0 {
          return (stack, variables, counter - ((n * -1) as usize));
        }
        return (stack, variables, counter + n as usize);
    }
    return (stack, variables, counter + 1);
}

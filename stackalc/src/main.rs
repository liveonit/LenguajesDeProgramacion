let mut stack : Vec<f64> = Vec::new();
let mut operations: [&str; 5] = ["SUB", "ADD", "MULT", "RET", "DIV"];

fn main() {

}
fn stackalc() {

    let mut lines = io::stdin().lock().lines();
    while let Some(line) = lines.next() {
        let last_input = line.unwrap();
        // stop storing the user input
        if last_input.len() == 0 {
            let item : f64 = match vec.pop() {
                Some(f64) => f64,
                None => break
            };
            println!("{:?}" , item);
        } else {
            for num in last_input.split_whitespace() {
                let n:f64 = num.parse().unwrap();
                vec.push(n);
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

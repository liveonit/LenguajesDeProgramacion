let mut stack : Vec<f64> = Vec::new();
let mut operations: [&str; 5] = ["SUB", "ADD", "MULT", "RET", "DIV"];

fn main() {

}

type op



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

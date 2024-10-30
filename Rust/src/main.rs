use rand_distr::{Normal, Distribution};

fn main() {
    const YINIT: f64 = 1.0;                              // initial population size
    const R: f64 = 1.4;                                  // maximum population growth rate
    const K: f64 = 20.0;                                 // carrying capacity
    const THETASD: f64 = 0.1;                            // std dev to add noise

    // 0th element of args is program name
    let args: Vec<String> = std::env::args().collect();
    
    let reps = args[1].parse::<usize>().unwrap();    
    // number of years of growth to simulate       
    let num_years = args[2].parse::<usize>().unwrap();  
    
    let mut results = Vec::with_capacity(reps);
    for _ in 0..reps {
        results.push(logmod(YINIT, R, K, THETASD, num_years));
    }
    println!("{:?}", results);
}

fn logmod(yinit: f64, r: f64, k: f64, thetasd: f64, t: usize) -> Vec<f64> {
    let mut ys = Vec::with_capacity(t);
    ys.push(yinit);
    for i in 1..t {
        let normal = Normal::new(0.0, thetasd).unwrap();
        let normal_draw = normal.sample(&mut rand::thread_rng());
        let ys_i = ys.get(i-1).unwrap() * 
        (r - r * (ys.get(i-1).unwrap() / k)) * normal_draw.exp();
        ys.push((ys_i * 100.0).round() / 100.0);
    }
    ys
}


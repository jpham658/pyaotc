use criterion::{criterion_group, criterion_main, Criterion, BenchmarkId};
use std::process::Command;

const BENCHMARK_FILES: &[&str; 11] = &[
    "examples/fib.py",
    "examples/hanoi.py",
    "examples/num_islands.py",
    "examples/factorial.py",
    "examples/list_add.py",
    "examples/iterating.py",
    "examples/subscript.py",
    "examples/collatz.py",
    "examples/range_iterate.py",
    "examples/append_lots.py",
    "examples/while.py",
];

fn compile_python_file(filename: &str) {
    let output = Command::new("./pyaotc.sh")
        .args(&[filename])
        .output()
        .expect("Failed to compile Python file");

    if !output.status.success() {
        eprintln!(
            "Compilation failed for {}: {}",
            filename,
            String::from_utf8_lossy(&output.stderr)
        );
        std::process::exit(1);
    }
}

fn benchmark_pyaotc(group: &mut criterion::BenchmarkGroup<'_, criterion::measurement::WallTime>, filename: &str) {
    compile_python_file(filename);

    let benchmark_name = format!("Pyaotc - {}", filename);
    let executable_name = filename
        .rsplitn(2, "/")
        .next()
        .unwrap_or("")
        .replace(".py", "");

    group.bench_with_input(BenchmarkId::new("Pyaotc", filename), filename, |b, _| {
        b.iter(|| {
            let output = Command::new(format!("./{}", executable_name))
                .output()
                .expect(format!("Failed to execute binary file ./{}.", executable_name).as_str());

            if !output.status.success() {
                eprintln!(
                    "Execution failed for {}: {}",
                    filename,
                    String::from_utf8_lossy(&output.stderr)
                );
            }
        });
    });
}

fn benchmark_python3(group: &mut criterion::BenchmarkGroup<'_, criterion::measurement::WallTime>, filename: &str) {
    group.bench_with_input(BenchmarkId::new("Python3", filename), filename, |b, _| {
        b.iter(|| {
            let output = Command::new("python3")
                .arg(filename)
                .output()
                .expect("Failed to execute python3");

            if !output.status.success() {
                eprintln!(
                    "Execution failed for {}: {}",
                    filename,
                    String::from_utf8_lossy(&output.stderr)
                );
            }
        });
    });
}

fn benchmark_all(c: &mut Criterion) {
    let mut group = c.benchmark_group("Benchmarks");

    for filename in BENCHMARK_FILES {
        benchmark_pyaotc(&mut group, filename);
        benchmark_python3(&mut group, filename);
    }

    group.finish();
}

criterion_group!{
    name = benches;
    config = Criterion::default().warm_up_time(std::time::Duration::new(6, 0)) 
                                 .measurement_time(std::time::Duration::new(30, 0))
                                 .sample_size(10);
    targets = benchmark_all
}
criterion_main!(benches);
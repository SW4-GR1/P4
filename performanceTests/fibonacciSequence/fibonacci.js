function fibonacci(n) {
    let a = 0;
    let b = 1;

    for(let i = 0; i < n; i++) {
        let temp = a;
        a = b;
        b = temp + b;
    }

    return a;
}
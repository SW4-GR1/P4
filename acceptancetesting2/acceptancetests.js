// tests = [
//     // ("forloop",["int", "int"])
//     // "gange",
//     // "icosahedronVolume",
//     // "modolus",
//     // "nestedForLoops",
//     // "sphereVolume",
//     ("squareRoot",["float"]),
//     // "sum",
//     // "toApower"
// ]
/* class test {
    
    constructor(name, args) {
        this.name = name;
        this.args = args;
    }
    
} */
tests = [
    {
        name : "squareRoot",
        // args : ["float"];
        args : "float",
        desc : "Finds the square root of a float"
    },
    {
        name : "doubleFloat",
        args : "float",
        desc : "Finds 2 times the amount of a float"
    }
]

// const fs = require('fs');
// const util = require('util');
// const readFile = util.promisify(fs.readFile);
let parent = document.getElementById('accept');
// Function to call a WebAssembly function from a file
async function callWasmFunction(wasmFile, inputval) {
    return new Promise((resolve, reject) => {
        const filename = "./tests/" + wasmFile.name + ".wasm";
        try {
            WebAssembly.instantiateStreaming(fetch(filename)).then(
                (obj) => {
                    const instance = obj.instance;
                    const functionNames = Object.keys(instance.exports);
                    const functions = instance.exports
                    
                    const args_str = console.log(`Give input values, expecting:${wasmFile.args}`);
                    if (functionNames.length > 0) {
                        const firstFunctionName = functionNames[0];
                        const result = functions[firstFunctionName](inputval);
                        console.log(`Result from ${firstFunctionName} in ${filename}: ${result}`);
                        let html = `<p>${filename}: ${wasmFile.desc} with input ${inputval}, result: ${result}</p>`;
                        html += `<input id="${wasmFile.name}Input" type="number" value="${inputval}">`;
                        html += `<button data-wasm-file='${JSON.stringify(wasmFile)}' onclick="runTest(this, '${wasmFile.name}Input')">Run Test</button>`;
                        
                        
                        resolve(html); // Resolve the promise here
                    } else {
                        console.log(`No exported functions in ${filename}`);
                        resolve(); // Resolve the promise here
                    }
                })
        } catch (error) {
            console.error(`Error calling function from ${filename}: ${error}`);
            reject(error); // Reject the promise here
        }
    });
}
async function runTest(buttonElement, inputID) {
    const wasmFile = JSON.parse(buttonElement.getAttribute('data-wasm-file'))
    const inputValue = parseFloat(document.getElementById(inputID).value);
    const result = await callWasmFunction(wasmFile, inputValue);
    document.getElementById(wasmFile.name).innerHTML = result;
}

// Call the WebAssembly function from each file in the list
// Call the WebAssembly function from each file in the list

Promise.all(tests.map(async test => {
    parent.innerHTML += `<div id=${test.name}></div>`;
    const result = await callWasmFunction(test, 25.0);
    document.getElementById(test.name).innerHTML = result;
    console.log(result);

})).then(() => {
    console.log(parent.innerHTML)
    console.log('All tests completed');
}).catch((error) => {
    console.error('An error occurred:', error);
});

// WebAssembly.instantiateStreaming(fetch("./icosahedronVolume/test.wasm")).then(
//     WebAssembly.instantiateStreaming(fetch("./icosahedronVolume/test.wasm")).then(
//     (obj) => {
//         result = obj.instance.exports.f(5.0);
//         test_div = document.getElementById('test')
//         test_div.textContent = result
//         console.log(result)
//     }
// ))
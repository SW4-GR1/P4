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
        args : ["float"],
        desc : "Finds the square root of a float"
    },
    {
        name : "doubleFloat",
        args : ["float"],
        desc : "Finds 2 times the amount of a float"
    },
    {
        name : "gange",
        args : ["int", "int"],
        desc : "Finds the product of two integers"
    },
    {
        name : "forloop_range",
        args : ["int", "int"],
        desc : "Finds the sum of a range of integers using a forloop",
    },
    {
        name : "icosahedronVolume",
        args : ["float"],
        desc : "Finds the volume of an icosahedron with radius float as input"
    },
    {
        name : "modolus",
        args : ["int", "int"],
        desc : "Finds the modolus of two integers using a while loop"
    },
    {
        name : "sumofpairs",
        args : ["int"],
        desc : "Finds the sum of pairs of integers from 1 to input n using 2 for loops",
    },
    {
        name : "sphereVolume",
        args : ["float"],
        desc : "Finds the volume of a sphere with radius float as input"
    },
    {
        name : "toApower",
        args : ["int", "int"],
        desc : "Finds the power of an integer to another integer using a forloop"
    },
    {
        name : "fibonacci",
        args : ["int"],
        desc : "Finds the nth fibonacci number using a while loop"
    }
]

// const fs = require('fs');
// const util = require('util');
// const readFile = util.promisify(fs.readFile);
let parent = document.getElementById('accept');
// Function to call a WebAssembly function from a file
async function callWasmFunction(wasmFile, inputvals) {
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
                        const result = functions[firstFunctionName](...inputvals);
                        console.log(`Result from ${firstFunctionName} in ${filename}: ${result}`);
                        let html = `<p>${filename}: ${wasmFile.desc} with input ${inputvals.join(', ')}, </p>`;
                        html += `<div style="display: flex; justify-content: flex-start">
                        <style>
                            div > * {
                                margin: 4px 12px;
                            }
                        </style>`;
                        wasmFile.args.forEach((arg, index) => {
                            html += `<input id="${wasmFile.name}Input${index}" type="number" value="${inputvals[index]}">`;
                        });
                        html += `<button data-wasm-file='${JSON.stringify(wasmFile)}' onclick="runTest(this, '${wasmFile.name}Input')">Run Test</button>`;
                        html += `<p> result: ${result} </p> </div>`
                        
                        
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
    const inputValues = wasmFile.args.map((arg, index) => {
        const inputValue = document.getElementById(inputID + index).value;
        return arg === 'int' ? parseInt(inputValue) : parseFloat(inputValue);
    });
    const result = await callWasmFunction(wasmFile, inputValues);
    document.getElementById(wasmFile.name).innerHTML = result;
}

// Call the WebAssembly function from each file in the list
// Call the WebAssembly function from each file in the list

Promise.all(tests.map(async test => {
    parent.innerHTML += `<div style="border: 1px solid black" id=${test.name}></div>`;
    const inputValues = test.args.map(arg => arg === 'int' ? 25 : 25.0);
    const result = await callWasmFunction(test, inputValues);
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
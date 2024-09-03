// let a = "outer";
// var a = a;
// console.log(a);
// let a = "outer";
//
// {
// 	let a = "i";
// 	console.log(a);
// }

// function fib(n) {
// 	if (n < 2) return n;
// 	return fib(n - 1) + fib(n - 2);
// }
//
// console.log(fib(25));

var a = "outer";
{
	var a = a;
}

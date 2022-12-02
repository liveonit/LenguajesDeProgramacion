const distance = (a, b) => a && b ? Math.sqrt(Math.abs(a.x - b.x) ** 2 + Math.abs((a.y - b.y)) ** 2) : NaN

console.log(distance({x: 5, y: 6}))
console.log(distance({x: 5, y: 6}, {x: -7}))
console.log(distance({x: 5, y: 6}, {x: -7, y: 11}))
console.log(distance({x: 5, y: 6}, {x: 5, y: 6}))
console.log(distance({x: 5, y: 6, z: 8}, {x: -7, y: 11, z: 14}))
    

function* range(start, end, step = 1) {
  const result = []
  for (let i = start; i < end && i > 0; i += step ) yield i;
}

console.log([...range(0,3)]);
console.log([...range(3,10)]);
console.log([...range(0,10,3)]);
console.log([...range(1,1)]);
console.log([...range(10,1,-1)]);
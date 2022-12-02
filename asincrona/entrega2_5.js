const delay = async (ms, result) =>
  new Promise((res) => {
    setTimeout(() => res(result ?? Date.now()), ms);
  });

// delay(100).then(console.log);
// delay(1000, "Resuelve pen ultimo ultimo").then(console.log);
// delay(200, "Resuelve segundo").then(console.log);
// delay(1100).then(console.log);

const asyncTryMany = async (arr, asyncFunc) =>
  Promise.allSettled(arr.map((v) => asyncFunc(v))).then((resolvedArr) =>
    resolvedArr.filter((r) => r.status !== "rejected").map((r) => r.value)
  );

// (async () => {
//   let r = await asyncTryMany([1,2,3], v => Promise.resolve(v))
//   console.log("result1", r)
//   r = await asyncTryMany([1,2,3], v => v % 2 ? Promise.resolve(v) : Promise.reject(new Error(v)))
//   console.log("result2", r)
// })()

const callOneByOne = async (iter, func) => {
  const result = [];
};

const asyncGoUntil = async (iter, func, timeout) => {
  const result = [];
  const timeoutPromise = new Promise(res => setTimeout(res, timeout));
  for (value of iter) {
    const r = await Promise.race([timeoutPromise, func(value)])
    if (!r)  break 
    result.push(r);
  }
  return result;
};

(async () => {
  let r = await asyncGoUntil([1, 2, 3], (t) => delay(t * 100, t), 350);
  console.log("result1", r);
  r = await asyncGoUntil("123", (t) => delay(t * 1000, t), 350);
  console.log("result2", r);
  r = await asyncGoUntil(new Set("123"), (t) => delay(t * 10, t), 350);
  console.log("result3", r);
  r = await asyncGoUntil("123", (t) => delay(t * 1000, t), 350);
  console.log("result2", r);
})();

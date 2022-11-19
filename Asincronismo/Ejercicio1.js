const distance = (a, b) => a && b ? Math.sqrt((a.x - b.x) ** 2 + (a.y - b.y) ** 2) : NaN

const range = (start, end, step) => {
  const result = []
  for (let i = start; i < end; i += step || 1 )
    result.push(i);
  return result;
}
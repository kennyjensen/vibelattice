// Port of AVL second.f: SECONDS returns current time in seconds.

function nowSeconds() {
  if (typeof performance !== 'undefined' && typeof performance.now === 'function') {
    return performance.now() / 1000;
  }
  return Date.now() / 1000;
}

export function SECONDS() {
  return nowSeconds();
}

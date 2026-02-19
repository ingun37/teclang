export function nonNull<A>(a: A | undefined | null): A {
  if (a === undefined || a === null) throw new Error("Expected non-null value");
  return a;
}

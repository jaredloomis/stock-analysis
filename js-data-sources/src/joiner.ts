import loadSource, { DataSource, parseSourceStr } from "./loader";

export default async function joinOn(sourceA: DataSource | string, sourceB: DataSource | string, joinColumn: string) {
  // Normalize source strings
  if(typeof(sourceA) === "string") {
    sourceA = parseSourceStr(sourceA);
  }
  if(typeof(sourceB) === "string") {
    sourceB = parseSourceStr(sourceB);
  }

  // Load sources
  const sourceObjectA = await loadSource(sourceA);
  const sourceObjectB = await loadSource(sourceB);

  return _joinOn(sourceObjectA, sourceObjectB, joinColumn);
}

function _joinOn<A, B>(a: A[], b: B[], joinColumn: string, aIsMaster=true): (A & B)[] {
  const bMap = b.reduce((acc, rowB) => {
    const joinVal = rowB[joinColumn];
    if(typeof joinVal !== "undefined" && joinVal !== null) {
      return Object.assign(acc, { [joinVal]: rowB });
    } else {
      console.warn(`Skipping row ${JSON.stringify(rowB)}`);
      return acc;
    }
  }, {});

  return a
    .map(rowA => {
      const joinVal = rowA[joinColumn];
      const rowB = bMap[joinVal];
      if(typeof joinVal !== "undefined" && joinVal !== null) {
        if(typeof rowB !== "undefined" && rowB !== null) {
          if(aIsMaster) {
            return Object.assign(rowB, rowA);
          } else {
            return Object.assign(rowA, rowB);
          }
        }
      } else {
        console.warn(`Skipping row ${JSON.stringify(rowB)}`);
        return null;
      }
    })
}

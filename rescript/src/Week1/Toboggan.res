let countTrees = (graph, (right, down)) => {
  let rec countWhileTraverse = (row, col, count) => {
    let height = graph->Slope.height
    let width = graph->Slope.width
    let isBounded = row >= height

    switch isBounded {
    | true => count
    | false => {
        let updatedCount = switch graph->Slope.getSlopeByCoords(row, col) {
        | Some(c) if c == "#" => count + 1
        | Some(_) => count
        | None => count
        }

        countWhileTraverse(row + down, mod(col + right, width), updatedCount)
      }
    }
  }
  countWhileTraverse(0, 0, 0)
}

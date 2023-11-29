def main(): Unit \ IO = {
    let input = "-a ^ b * c ^ d + e ^ f / g ^ (h - i)";
    Examples.Arithmetic.parse(input) |> println
}

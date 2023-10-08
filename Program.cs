using CSharpToSwift;

Console.WriteLine("C# to Swift Transpiler");

static void PrintHelp() {
    Console.WriteLine("Usage: csharp2swift <path-to-csproj> <output-dir>");
}

if (args.Length < 2) {
    PrintHelp();
    return 1;
}
string projectFilePath = args[0];
if (!File.Exists(projectFilePath)) {
    Console.WriteLine($"Project file {projectFilePath} does not exist.");
    return 1;
}
string outputDir = args[1];
if (!Directory.Exists(outputDir)) {
    Console.WriteLine($"Output directory {outputDir} does not exist.");
    return 1;
}

try {
    var transpiler = new Transpiler(projectFilePath, outputDir);
    await transpiler.TranspileAsync();
    return 0;
}
catch (Exception ex) {
    Console.WriteLine(ex.Message);
    return 2;
}

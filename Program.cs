using CSharpToSwift;

Console.WriteLine("C# to Swift Transpiler");

string projectFilePath = "/Users/fak/Dropbox/Projects/Circuit/CircuitGalleryLib/CircuitLib.csproj";

try {
    var transpiler = new Transpiler(projectFilePath);
    await transpiler.TranspileAsync();
}
catch (Exception ex) {
    Console.WriteLine(ex.Message);
}


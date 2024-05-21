# Get the list of files in the directory
$files = Get-ChildItem -Path .\acceptancetesting2\tests

# Loop through each file
foreach ($file in $files) {
    # Execute numatix.exe on the file
    .\numatix.exe $file.FullName
}

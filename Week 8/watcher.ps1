# watch a file changes in the current directory, 
# execute all tests when a file is changed or renamed

$watcher = New-Object System.IO.FileSystemWatcher
$watcher.Path = get-location
$watcher.IncludeSubdirectories = $true
$watcher.EnableRaisingEvents = $false
$watcher.NotifyFilter = [System.IO.NotifyFilters]::LastWrite -bor [System.IO.NotifyFilters]::FileName

write-host "Watching " $watcher.Path

while($TRUE){
	$result = $watcher.WaitForChanged([System.IO.WatcherChangeTypes]::Changed -bor [System.IO.WatcherChangeTypes]::Renamed -bOr [System.IO.WatcherChangeTypes]::Created);
	if($result.TimedOut){
		Write-Host "TIMEOUT"
		continue;
	}
    Clear-Host
    write-host "Testing " $result.Name
	doctest.exe $result.Name
}
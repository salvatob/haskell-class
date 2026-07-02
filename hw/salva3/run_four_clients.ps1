$executable = "C:\Users\tobia\HaskellProjects\haskell-class\dist-newstyle\build\x86_64-windows\ghc-9.4.8\logo-webapp-0.1.0.0\x\client\build\client\client.exe"
$windows_screen_scaling_factor = 1.25

[int]$half_sw = 1920 / 2 / $windows_screen_scaling_factor
[int]$half_sd = 1080 / 2 / $windows_screen_scaling_factor

$positions = @(
    @(0, 0),
    @($half_sw, 0),
    @(0, $half_sd),
    @($half_sw, $half_sd)
)

foreach ($p in $positions) {
    Start-Process $executable -ArgumentList @("-x", $p[0],"-y", $p[1],"-w", $half_sw,"-h", $half_sd)
}
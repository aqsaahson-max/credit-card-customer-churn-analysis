# Simple PowerShell HTTP Server for the dashboard
# Usage: powershell -ExecutionPolicy Bypass -File serve.ps1

$port = 8000
$root = Split-Path -Parent $MyInvocation.MyCommand.Path

$mimeTypes = @{
    '.html' = 'text/html; charset=utf-8'
    '.css'  = 'text/css; charset=utf-8'
    '.js'   = 'application/javascript; charset=utf-8'
    '.json' = 'application/json; charset=utf-8'
    '.csv'  = 'text/csv; charset=utf-8'
    '.png'  = 'image/png'
    '.jpg'  = 'image/jpeg'
    '.svg'  = 'image/svg+xml'
    '.ico'  = 'image/x-icon'
}

$listener = New-Object System.Net.HttpListener
$listener.Prefixes.Add("http://localhost:$port/")

try {
    $listener.Start()
    Write-Host ""
    Write-Host "  Dashboard server running at: http://localhost:$port" -ForegroundColor Cyan
    Write-Host "  Serving from: $root" -ForegroundColor DarkGray
    Write-Host "  Press Ctrl+C to stop" -ForegroundColor DarkGray
    Write-Host ""

    while ($listener.IsListening) {
        $context = $listener.GetContext()
        $request = $context.Request
        $response = $context.Response

        $path = $request.Url.LocalPath
        if ($path -eq '/') { $path = '/index.html' }

        $filePath = Join-Path $root ($path.TrimStart('/').Replace('/', '\'))

        try {
            if (Test-Path $filePath -PathType Leaf) {
                $ext = [System.IO.Path]::GetExtension($filePath)
                $contentType = $mimeTypes[$ext]
                if (-not $contentType) { $contentType = 'application/octet-stream' }

                $response.ContentType = $contentType
                $response.StatusCode = 200

                $bytes = [System.IO.File]::ReadAllBytes($filePath)
                $response.ContentLength64 = $bytes.LongLength
                $response.OutputStream.Write($bytes, 0, $bytes.Length)

                Write-Host "  200  $path  ($($bytes.Length) bytes)" -ForegroundColor Green
            } else {
                $response.StatusCode = 404
                $response.ContentType = 'text/plain; charset=utf-8'
                $msg = [System.Text.Encoding]::UTF8.GetBytes("404 Not Found: $path")
                $response.ContentLength64 = $msg.LongLength
                $response.OutputStream.Write($msg, 0, $msg.Length)

                Write-Host "  404  $path" -ForegroundColor Red
            }
        } catch {
            Write-Host "  ERR  $path - $_" -ForegroundColor Yellow
        } finally {
            try { $response.OutputStream.Close() } catch {}
        }
    }
} catch {
    Write-Host "Error: $_" -ForegroundColor Red
} finally {
    $listener.Stop()
    Write-Host "Server stopped." -ForegroundColor Yellow
}

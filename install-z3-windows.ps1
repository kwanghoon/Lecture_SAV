# Z3 4.8.12 설치 스크립트 (Windows)
# PowerShell 스크립트

# UTF-8 인코딩 설정 (한글 깨짐 방지)
# [Console]::OutputEncoding = [System.Text.Encoding]::UTF8
# $OutputEncoding = [System.Text.Encoding]::UTF8
# chcp 65001 | Out-Null

$Z3_VERSION = "4.8.12"
$Z3_DIR = "D:\z3-$Z3_VERSION-x64-win"
$DOWNLOAD_URL = "https://github.com/Z3Prover/z3/releases/download/z3-$Z3_VERSION/z3-$Z3_VERSION-x64-win.zip"
$TEMP_ZIP = "$env:TEMP\z3-$Z3_VERSION-x64-win.zip"

Write-Host "Z3 $Z3_VERSION 설치를 시작합니다..." -ForegroundColor Green

# 이미 설치되어 있는지 확인
if (Test-Path $Z3_DIR) {
    Write-Host "Z3가 이미 $Z3_DIR 에 설치되어 있습니다." -ForegroundColor Yellow
    $response = Read-Host "다시 설치하시겠습니까? (y/n)"
    if ($response -ne "y") {
        Write-Host "설치를 취소합니다." -ForegroundColor Red
        exit
    }
    Remove-Item -Recurse -Force $Z3_DIR
}

# Z3 다운로드
Write-Host "Z3 다운로드 중: $DOWNLOAD_URL" -ForegroundColor Cyan
try {
    Invoke-WebRequest -Uri $DOWNLOAD_URL -OutFile $TEMP_ZIP
    Write-Host "다운로드 완료" -ForegroundColor Green
} catch {
    Write-Host "다운로드 실패: $_" -ForegroundColor Red
    exit 1
}

# 압축 해제
Write-Host "압축 해제 중..." -ForegroundColor Cyan
try {
    # D:\ 드라이브에 압축 해제
    Expand-Archive -Path $TEMP_ZIP -DestinationPath "D:\" -Force
    Write-Host "압축 해제 완료: $Z3_DIR" -ForegroundColor Green
} catch {
    Write-Host "압축 해제 실패: $_" -ForegroundColor Red
    Remove-Item $TEMP_ZIP -ErrorAction SilentlyContinue
    exit 1
}

# 임시 파일 정리
Remove-Item $TEMP_ZIP -ErrorAction SilentlyContinue

# 설치 확인
if (Test-Path "$Z3_DIR\bin\z3.exe") {
    Write-Host "`nZ3 설치 완료!" -ForegroundColor Green
    Write-Host "설치 위치: $Z3_DIR" -ForegroundColor Cyan
    Write-Host "bin 디렉토리: $Z3_DIR\bin" -ForegroundColor Cyan
    Write-Host "include 디렉토리: $Z3_DIR\include" -ForegroundColor Cyan
    Write-Host "`nWhile 언어 프로젝트 빌드 시 다음 명령어를 사용하세요:" -ForegroundColor Yellow
    Write-Host "stack build --extra-include-dirs=$Z3_DIR\include --extra-lib-dirs=$Z3_DIR\bin" -ForegroundColor White
} else {
    Write-Host "설치 실패: z3.exe를 찾을 수 없습니다." -ForegroundColor Red
    exit 1
}
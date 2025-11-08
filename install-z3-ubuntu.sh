#!/bin/bash
# Z3 4.8.12 설치 스크립트 (Ubuntu/Linux)

Z3_VERSION="4.8.12"
INSTALL_DIR="/usr/local"
DOWNLOAD_URL="https://github.com/Z3Prover/z3/releases/download/z3-${Z3_VERSION}/z3-${Z3_VERSION}-x64-glibc-2.31.zip"
TEMP_DIR="/tmp/z3-install"
TEMP_ZIP="${TEMP_DIR}/z3-${Z3_VERSION}.zip"

echo "Z3 ${Z3_VERSION} 설치를 시작합니다..."

# 임시 디렉토리 생성
mkdir -p ${TEMP_DIR}

# Z3 다운로드
echo "Z3 다운로드 중: ${DOWNLOAD_URL}"
if ! wget -O ${TEMP_ZIP} ${DOWNLOAD_URL}; then
    echo "다운로드 실패"
    rm -rf ${TEMP_DIR}
    exit 1
fi
echo "다운로드 완료"

# 압축 해제
echo "압축 해제 중..."
if ! unzip -q ${TEMP_ZIP} -d ${TEMP_DIR}; then
    echo "압축 해제 실패"
    rm -rf ${TEMP_DIR}
    exit 1
fi

# 설치 (관리자 권한 필요)
echo "Z3 설치 중 (관리자 권한 필요)..."
Z3_EXTRACTED_DIR="${TEMP_DIR}/z3-${Z3_VERSION}-x64-glibc-2.31"

if [ ! -d "${Z3_EXTRACTED_DIR}" ]; then
    # 다른 가능한 디렉토리 이름 확인
    Z3_EXTRACTED_DIR=$(find ${TEMP_DIR} -maxdepth 1 -type d -name "z3-*" | head -n 1)
    if [ -z "${Z3_EXTRACTED_DIR}" ]; then
        echo "압축 해제된 Z3 디렉토리를 찾을 수 없습니다."
        rm -rf ${TEMP_DIR}
        exit 1
    fi
fi

# 바이너리 설치
sudo cp ${Z3_EXTRACTED_DIR}/bin/z3 /usr/local/bin/
sudo chmod +x /usr/local/bin/z3

# 라이브러리 설치
sudo cp ${Z3_EXTRACTED_DIR}/bin/libz3.so* /usr/local/lib/ 2>/dev/null || \
sudo cp ${Z3_EXTRACTED_DIR}/bin/libz3.so /usr/local/lib/

# 헤더 파일 설치
sudo mkdir -p /usr/local/include/z3
sudo cp ${Z3_EXTRACTED_DIR}/include/* /usr/local/include/z3/

# ldconfig 실행하여 라이브러리 캐시 업데이트
sudo ldconfig

# 임시 파일 정리
rm -rf ${TEMP_DIR}

# 설치 확인
if command -v z3 &> /dev/null; then
    echo ""
    echo "Z3 설치 완료!"
    echo "설치된 Z3 버전:"
    z3 --version
    echo ""
    echo "라이브러리 위치: /usr/local/lib"
    echo "헤더 파일 위치: /usr/local/include/z3"
    echo ""
    echo "또는 apt를 통한 설치도 가능합니다:"
    echo "  sudo apt-get install libz3-dev"
else
    echo "설치 실패: z3 명령어를 찾을 수 없습니다."
    exit 1
fi

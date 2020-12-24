#!/usr/bin/env python3

print('''
matrix:
  include:''')

def language_cpp(**kwargs):
    print('''\
    - language: cpp
      compiler: {compiler}
      install:
        - DEPS_DIR="${{TRAVIS_BUILD_DIR}}/deps"
        - mkdir ${{DEPS_DIR}} && cd ${{DEPS_DIR}}
        - |
          if [[ "${{TRAVIS_OS_NAME}}" == "linux" ]]; then
            CMAKE_URL="http://www.cmake.org/files/v3.5/cmake-3.5.1-Linux-x86_64.tar.gz"
            mkdir cmake && travis_retry wget --no-check-certificate --quiet -O - ${{CMAKE_URL}} | tar --strip-components=1 -xz -C cmake
            export PATH=${{DEPS_DIR}}/cmake/bin:${{PATH}}
          fi
        - cd ..
      script:
        - mkdir build
        - cd build
        - ${{DEPS_DIR}}/cmake/bin/cmake ..
        - make
        - make test
        - make examples'''.format(**kwargs))

language_cpp(compiler='gcc')
language_cpp(compiler='clang')

def language_python(**kwargs):
    print('''\
    - language: python
      python: {version}
      install:
        - python setup.py install
      script:
        - python setup.py test
        - cd examples
        - ./exercisers.py'''.format(**kwargs))

PYTHON_VERSIONS = [
    '3.4',
    '3.5', '3.5-dev',
    '3.6', '3.6-dev',
    '3.7', '3.7-dev',
    '3.8', '3.8-dev',
    '3.9-dev',
]

for version in PYTHON_VERSIONS:
    language_python(version=version)

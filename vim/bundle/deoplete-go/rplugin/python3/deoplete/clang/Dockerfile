FROM buildpack-deps:wily
MAINTAINER zchee <k@zchee.io>

ENV LLVM_VERSION=3.7 \
	PYTHONPATH=/llvm/tools/clang/bindings/python \
	LD_LIBRARY_PATH=$("llvm-config-$LLVM_VERSION --libdir")

RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
		clang-$LLVM_VERSION \
		llvm-$LLVM_VERSION-dev \
		python3-dev \
		python3-pip \
	&& rm -rf /var/lib/apt/lists/* \
	\
	&& ln -s \
		/usr/lib/x86_64-linux-gnu/libclang-$LLVM_VERSION.so.1 /usr/lib/x86_64-linux-gnu/libclang.so \
	&& pip3 install nose

COPY . /llvm/tools/clang/bindings/python/
WORKDIR /llvm/tools/clang/bindings/python

RUN python3 setup.py install

CMD ["nosetests", "-v"]

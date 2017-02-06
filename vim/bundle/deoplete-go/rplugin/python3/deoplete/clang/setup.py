#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from setuptools import setup

setup(name="libclang-py3",
      version="0.3",
      description="Python3 bindings for libclang ",
      url="https://bitbucket.org/Anteru/python3-libclang",
      download_url="https://bitbucket.org/Anteru/python3-libclang/releases",
      license="License :: OSI Approved :: University of Illinois/NCSA Open Source License",
      classifiers=[
          "Intended Audience :: Developers",
          "License :: OSI Approved :: BSD License",
          "Programming Language :: Python",
          "Development Status :: 5 - Production/Stable",
          "Topic :: Software Development :: Compilers"
      ],
      keywords=["llvm", "clang", "libclang"],
      author="Matthaeus G. Chajdas",
      author_email="dev@anteru.net",
      zip_safe=False,
      packages=["clang"]
)

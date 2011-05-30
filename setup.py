# -*- coding: utf-8 -*-

from setuptools import setup, find_packages

setup(
    name = 'py2erl',
    version = '0.0.1',
    author = 'Oleksandr Lysenko',
    author_email = 'sashkolysenko@gmail.com',
    description = ('Primitive and naive Python to Erlang compiler'),
    packages = find_packages(),
    include_package_data=True,
    package_data = {'py2erl': ['erl/*.erl', 'test_files/*.py']},
)

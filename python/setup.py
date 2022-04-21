import setuptools

def get_long_description():
    with open("README.md", "r") as readme_file:
        return readme_file.read()

setuptools.setup(
    name="sodiumfrp",
    version="1.0.0",
    author="Stephen Blackheath",
    description= \
        "Python implementation of Sodium - "
        "Functional Reactive Programming (FRP) library",
    long_description=get_long_description(),
    long_description_content_type="text/markdown",
    url="https://github.com/SodiumFRP/sodium",
    packages=setuptools.find_packages(),
    package_data={"sodiumfrp": ["py.typed"]},
    license="BSD-3 License",
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: BSD License",
        "Operating System :: OS Independent",
    ],
    python_requires=">=3.6"
)

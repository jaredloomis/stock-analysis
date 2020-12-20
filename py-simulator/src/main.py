import subprocess
from os.path import abspath


def get_shell_cmd():
    return "/usr/bin/bash"


def get_data_loader_cmd():
    """
    :return: command to run data loader
    """
    return [get_shell_cmd(), '-c', '"gradle run"']


def get_data_loader_dir():
    return abspath("../../kt-data-loader")


def load_samples(spec):
    """
    Load samples from data loader
    :param spec:
    :return: stream of samples
    """
    process = subprocess.Popen(get_data_loader_cmd(),
                               stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE,
                               cwd=get_data_loader_dir())
    stdout, stderr = process.communicate()
    return stdout, stderr


print(load_samples(""))

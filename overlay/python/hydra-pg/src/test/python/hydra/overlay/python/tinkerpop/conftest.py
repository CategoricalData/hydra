def pytest_configure(config):
    config.addinivalue_line(
        "markers",
        "integration: tests requiring a running Gremlin Server (skipped by default)",
    )

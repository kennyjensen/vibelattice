const config = {
  testDir: './js/playwright',
  timeout: 30000,
  use: {
    browserName: 'chromium',
    headless: true,
  },
};

export default config;

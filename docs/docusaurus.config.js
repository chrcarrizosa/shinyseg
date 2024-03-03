// @ts-check
// `@type` JSDoc annotations allow editor autocompletion and type checking
// (when paired with `@ts-check`).
// There are various equivalent ways to declare your Docusaurus config.
// See: https://docusaurus.io/docs/api/docusaurus-config

import {themes as prismThemes} from 'prism-react-renderer';

/** @type {import('@docusaurus/types').Config} */
const config = {
  // Base
  title: 'shinyseg',
  url: 'https://chrcarrizosa.github.io',
  baseUrl: '/shinyseg/',
  // GitHub pages deployment
  organizationName: 'chrcarrizosa',
  projectName: 'shinyseg',
  // Other
  trailingSlash: false,
  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',
  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  presets: [
    [
      'classic',
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          routeBasePath: '/',
          sidebarPath: './sidebars.js',
          sidebarCollapsed: false,
        },
        blog: false,
        theme: {
          customCss: './src/css/custom.css',
        },
      }),
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      colorMode: {
        disableSwitch: true
      },
      navbar: {
        title: 'shinyseg',
        hideOnScroll: true,
        items: [
          {
          type: 'html',
          position: 'left',
          value: 'A web application for clinical cosegregation analysis',
          },
          {
          to: 'https://chrcarrizosa.shinyapps.io/shinyseg',
          label: 'To the app',
          position: 'right',
          className: 'button button--secondary button'
          },
          {
            href: 'https://github.com/chrcarrizosa/shinyseg',
            'aria-label': 'GitHub',
            className: 'navbar--github-link',
            position: 'right',
          },
        ],
      },
      footer: {
        style: 'dark',
        copyright: `Site built with Docusaurus.`,
      },
      prism: {
        theme: prismThemes.github,
        darkTheme: prismThemes.dracula,
      },
    }),
};

export default config;

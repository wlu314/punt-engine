import { QuartzConfig } from "./quartz/cfg"
import * as Plugin from "./quartz/plugins"

/**
 * Quartz 4.0 Configuration
 *
 * See https://quartz.jzhao.xyz/configuration for more information.
 */
const config: QuartzConfig = {
  configuration: {
    pageTitle: "Punt Engine",
    pageTitleSuffix: "",
    enableSPA: true,
    enablePopovers: true,
    analytics: {
      provider: "plausible",
    },
    locale: "en-US",
    baseUrl: "punt-engine.com",
    ignorePatterns: ["private", "templates", ".obsidian"],
    defaultDateType: "created",
    theme: {
      fontOrigin: "googleFonts",
      cdnCaching: true,
      typography: {
        header: "DM Serif Display",
        body: "Inter",
        code: "DM Mono",
      },
      colors: {
        lightMode: { //this is actually dark mode
          light: "#0a0a09",
          lightgray: "#303030",
          gray: "#dddddd",
          darkgray: "#e4e4e4",
          dark: "#fffff",
          secondary: "#59C2FF",
          tertiary: "#4963a6",
          highlight: "rgba(143, 159, 169, 0.15)",
          textHighlight: "",
        },
        darkMode: {
          light: "#0B0E14",
          lightgray: "#303030",
          gray: "#dddddd",
          darkgray: "#e4e4e4",
          dark: "#fffff",
          secondary: "#82AAFF",
          tertiary: "#d97e3d",
          highlight: "rgba(143, 159, 169, 0.15)",
          textHighlight: "",
        },
      },
    },
  },
  plugins: {
    transformers: [
      Plugin.FrontMatter(),
      Plugin.CreatedModifiedDate({
        priority: ["frontmatter", "filesystem"],
      }),
      Plugin.SyntaxHighlighting({
        theme: {
          light: "material-theme-darker",
          dark: "vitesse-light",
        },
        keepBackground: false,
      }),
      Plugin.ObsidianFlavoredMarkdown({ enableInHtmlEmbed: false }),
      Plugin.GitHubFlavoredMarkdown(),
      Plugin.TableOfContents(),
      Plugin.CrawlLinks({ markdownLinkResolution: "shortest" }),
      Plugin.Description(),
      Plugin.Latex({ renderEngine: "katex" }),
    ],
    filters: [Plugin.RemoveDrafts()],
    emitters: [
      Plugin.AliasRedirects(),
      Plugin.ComponentResources(),
      Plugin.ContentPage(),
      Plugin.FolderPage(),
      Plugin.TagPage(),
      Plugin.ContentIndex({
        enableSiteMap: true,
        enableRSS: true,
      }),
      Plugin.Assets(),
      Plugin.Static(),
      Plugin.NotFoundPage(),
    ],
  },
}

export default config

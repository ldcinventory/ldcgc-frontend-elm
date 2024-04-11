# ldcgc-frontend-elm

[![Elm CI](https://github.com/ldcinventory/ldcgc-frontend-elm/workflows/Elm%20CI/badge.svg)](https://github.com/ldcinventory/ldcgc-frontend-elm/actions)
[![Netlify Status](https://api.netlify.com/api/v1/badges/433924af-2aa7-45a8-9ec7-300c96277e87/deploy-status)](https://app.netlify.com/sites/ldcgc-frontend/deploys)

> Built with [Elm Land](https://elm.land) 🌈

## Local development

```bash
npm start
```

## Deploying to production

Elm Land projects are most commonly deployed as static websites.

Please visit [the "Deployment" guide](https://elm.land/guide/deploying) to learn more
about deploying your app for free using Netlify or Vercel.

## TODO:

- [x] Remove the users/me endpoint (login returns that)
- [x] Add optional name from volunteers.name
- [x] Remove tailwind modules and use barebones tailwindcss
- [x] Pass `NODE_ENV` variables to Elm-Land 🌈
- [x] Add EULA acceptance pages
- [x] Encode/Decode Tokens as a standalone type
- [x] Put back dark mode in sign in page and sidebar!
- [x] Customise NotFound 404 page
- [x] Extract Components/Button and re-use it!
- [x] Implement responsive sidebar menu
- [x] Implement volunteers page
- [ ] Implement password recovery workflow
- [ ] Only store user in localStorage if ✅ Remember me
- [ ] Extract Components/Modal and re-use it!
- [ ] Implement tools page
- [ ] Implement consumables page

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

- [ ] Implement password recovery workflow
- [ ] Convince backend to use Bearer token and simplify all API calls... 🙏🏻
- [ ] Check expiry token on session reconnect and redirect to Sign in if expired
- [ ] Only store user in localStorage if ✅ Remember me
- [ ] Implement tools page
- [ ] Implement register tools tab (register page)
- [x] Implement register consumables tab (register page)
- [ ] Use [elm-select](https://github.com/sporto/elm-select/tree/6.2.1) for Registering tools/consumables
- [ ] Extract Components/Modal and re-use it!

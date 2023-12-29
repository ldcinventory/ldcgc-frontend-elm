export const flags = ({ env }) => {
  // Called before our Elm application starts
  const [signatureToken, headerPayloadToken] = JSON.parse(
    window.localStorage.token || [null, null]
  );
  return {
    signatureToken,
    headerPayloadToken,
  };
};

export const onReady = ({ env, app }) => {
  // Called after our Elm application starts
  if (app.ports && app.ports.sendToLocalStorage) {
    app.ports.sendToLocalStorage.subscribe(({ key, value }) => {
      window.localStorage[key] = JSON.stringify(value);
    });
  }
};

function get(): Promise<Array<Person>> {
  return fetch(withRemoteBaseUrl(``))
}
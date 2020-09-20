# Ambiorix

See [how to deploy](https://ambiorix.john-coene.com/#/guide/deploy) ambiorix for more details.


```bash
docker pull jcoenep/ambiorix
```

By default the image will use an internal very basic application and binds it to port `3000`.

```bash
docker run -p 3000:3000 jcoenep/ambiorix
```

To use your own application mount your app in the images `/app` directory, so from the root of the app this would look like this:

```bash
docker run -v "$(pwd)"/.:/app -p 3000:3000 jcoenep/ambiorix
```

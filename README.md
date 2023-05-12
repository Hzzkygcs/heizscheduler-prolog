# HeizScheduler

## Members 
- Immanuel - 2006463162   
- Pradipta Davi Valendra - 2006462664
- Tara Mazaya Lababan - 2006473535
- Pramudiptha - 2006482426 

## Deployment
https://heizscheduler.herokuapp.com/

## Getting Started (Django Server)

Untuk menjalankan Django Server, jalankan command docker:

```bash
docker build --tag heizscheduler --progress=plain .
docker run --name heizscheduler -p 80:8000 heizscheduler
```

Jika ada error, pastikan tidak ada image yang bernama `heizscheduler`.
Alternatif lain, teman-teman bisa langsung mencbanya pada link deployment di atas. 

## Getting Started (Directly IO Prolog)

Untuk menjalankan secara langsung tanpa UI, jalankan file `main.pl` 
menggunakan SWI-Prolog. Menu input output akan ditampilkan secara otomatis.

## Github
https://github.com/Hzzkygcs/heizscheduler-prolog
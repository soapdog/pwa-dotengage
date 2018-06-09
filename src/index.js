import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

let data = localStorage.getItem('data');
data = data ? JSON.parse(data) : false;
console.log('data', data)
let app = Main.embed(document.getElementById('root'), data);

app.ports.openWindow.subscribe(function (newSite) {
  window.open(newSite);
});


app.ports.saveData.subscribe(function (data) {
  console.log('saving data' + Math.random(), data)
  localStorage.setItem('data', data);
});

registerServiceWorker();

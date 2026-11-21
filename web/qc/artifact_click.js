/* RC28.25.9BCD.6 — click an existing Results Library row to get download options.
   No new file URL is constructed. The existing authorized Shiny download handler
   remains the only download endpoint. */
(function(){
  'use strict';
  if(window.__srmaQcArtifactClickInstalled)return;
  window.__srmaQcArtifactClickInstalled=true;
  var pop=null, selected=null;
  function close(){if(pop){pop.remove();pop=null;}}
  function normalize(x){return String(x||'').toLocaleLowerCase().replace(/\s+/g,' ').trim();}
  function optionList(){
    var sel=document.getElementById('artifact');
    if(!sel)return [];
    return Array.from(sel.options).filter(function(o){return o.value && !o.disabled;});
  }
  function show(row){
    close();
    var items=optionList();
    pop=document.createElement('div');pop.id='srma-artifact-menu';pop.setAttribute('role','dialog');
    pop.setAttribute('aria-label','Download result');
    pop.style.cssText='position:fixed;z-index:3000;max-width:min(420px,94vw);width:340px;background:#fff;color:#17233c;border:1px solid #cbd5e1;border-radius:8px;box-shadow:0 12px 38px #0002;padding:13px;';
    var ttl=document.createElement('strong');ttl.textContent='Download result';pop.appendChild(ttl);
    var hint=document.createElement('p');hint.textContent='Choose the registered output. The original version and access checks are preserved.';
    hint.style.cssText='font-size:12px;color:#64748b;margin:6px 0';pop.appendChild(hint);
    var menu=document.createElement('select');menu.setAttribute('aria-label','Available outputs');
    menu.style.cssText='width:100%;min-width:0;padding:7px;margin-bottom:10px;border:1px solid #ccc;';
    var rowText=normalize(row.textContent),best=null,score=0;
    items.forEach(function(o){
      var cloned=document.createElement('option');cloned.value=o.value;cloned.textContent=o.textContent;
      menu.appendChild(cloned);
      var words=normalize(o.textContent).split(/[\s\/_.()\-]+/).filter(function(w){return w.length>=4;});
      var s=words.filter(function(w){return rowText.indexOf(w)!==-1;}).length;
      if(s>score){best=o.value;score=s;}
    });
    var existing=document.getElementById('artifact');
    if(existing && items.some(function(o){return o.value===existing.value;}))menu.value=existing.value;
    if(best&&score>=2)menu.value=best;
    if(!items.length){
      var empty=document.createElement('p');empty.textContent='No registered downloadable file is available for this version.';
      pop.appendChild(empty);
    }
    pop.appendChild(menu);
    var choose=document.createElement('button');choose.textContent='Select file';choose.className='btn btn-outline-secondary btn-sm';
    choose.disabled=!items.length;
    choose.onclick=function(){
      var original=document.getElementById('artifact');if(!original)return;
      original.value=menu.value;
      original.dispatchEvent(new Event('change',{bubbles:true}));
      selected=menu.value;
      var note=document.getElementById('srma-artifact-note');
      if(note)note.textContent='Selected. Click Download below after the selection updates.';
    };
    var download=document.createElement('button');download.textContent='Download selected';download.className='btn btn-primary btn-sm';
    download.style.marginLeft='8px';download.disabled=!items.length;
    download.onclick=function(){
      var original=document.getElementById('artifact'),link=document.getElementById('download');
      if(!original||!link||!menu.value)return;
      if(original.value!==menu.value || selected!==menu.value){
        original.value=menu.value;
        original.dispatchEvent(new Event('change',{bubbles:true}));
        selected=menu.value;
        var note=document.getElementById('srma-artifact-note');
        if(note)note.textContent='Selected. Click Download selected again once Shiny has updated.';
        return;
      }
      // A separate real click, after Shiny has received the selected artifact,
      // invokes the existing authorized download route.
      link.click();
    };
    pop.appendChild(choose);pop.appendChild(download);
    var n=document.createElement('p');n.id='srma-artifact-note';n.style.cssText='font-size:12px;color:#64748b;margin:8px 0 0';
    n.textContent='Choose a file, then download. Nothing is regenerated.';pop.appendChild(n);
    document.body.appendChild(pop);
    var rect=row.getBoundingClientRect();
    pop.style.left=Math.max(8,Math.min(window.innerWidth-355,rect.left))+'px';
    pop.style.top=Math.max(8,Math.min(window.innerHeight-230,rect.bottom+5))+'px';
    menu.focus();
  }
  document.addEventListener('click',function(e){
    if(pop&&pop.contains(e.target))return;
    var row=e.target.closest&&e.target.closest('#results_library_table tbody tr');
    if(row){show(row);return;}
    if(pop)close();
  });
  document.addEventListener('keydown',function(e){
    if(e.key==='Escape')close();
    if(e.key==='Enter'&&e.target.matches&&e.target.matches('#results_library_table tbody tr'))show(e.target);
  });
  function accessibility(){
    document.querySelectorAll('#results_library_table tbody tr').forEach(function(tr){
      tr.tabIndex=0;tr.setAttribute('role','button');tr.title='Click for download options';
      tr.style.cursor='pointer';
    });
  }
  var obs=new MutationObserver(accessibility);
  obs.observe(document.documentElement,{childList:true,subtree:true});
  accessibility();
})();

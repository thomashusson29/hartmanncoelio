# Avis détaillé sur `Hartmann_Coelio_v4_discussion.docx`

Repère de lecture : les numéros `PP###` renvoient au fichier d’extraction `Hartmann_Coelio_v4_discussion_paragraphs.md` (même ordre que le `.docx`, avec des marqueurs `[TABLE]` à l’emplacement des tableaux).

## Points majeurs (priorité haute)

- Brouillon non “soumission-ready” : passages en français à supprimer/intégrer (`PP071–PP077`), placeholders “Tableau …” (`PP050`, `PP055`) et note résiduelle (`PP046`, `PP081`).
- Incohérences chiffrées à corriger partout : `n=285` vs `n=284` (`PP018`, `PP044`, Table 1), `OPA 147` vs `146`, p-values (ex. morbidité `p=0.8` vs `0.6`) et surtout HR (`2.28` dans l’abstract vs `1.84` dans les résultats/Table 4).
- Robustesse statistique : gestion des manquants par “pairwise deletion” (`PP039`) + censure du décès comme simple censure (`PP033`) → risque de biais (competing risk, MNAR), à discuter/renforcer.
- Présentation des tableaux : libellés “Not concerned”, lignes incomplètes (“Actual diverticulitis”), p-values après appariement (souvent déconseillées), décimales/ponctuation incohérentes.

## Commentaires paragraphe par paragraphe

### Titre / auteurs / métadonnées

- `PP001` (Titre) — Fond : question claire, mais formulation un peu “journalistique”; envisager un titre plus descriptif si la revue est stricte. Cohérence : “acute colonic diverticulitis” vs “perforated sigmoid diverticulitis” (termes à harmoniser avec le corps).
- `PP002` — Forme : OK.
- `PP003` — Forme : l’ellipse “…” doit disparaître (liste complète, affiliations, contributions/ORCID selon revue).
- `PP004` — Fond : affiliation OK; Forme : homogénéiser langue (français/anglais) selon exigences de la revue.
- `PP005` — Forme : “2.” vide (à supprimer ou compléter).
- `PP006` — Forme : OK (à homogénéiser avec le style global).
- `PP007` — Forme : OK; vérifier conventions (MD/PhD) selon la revue.
- `PP008` — Forme : OK.
- `PP009` — Forme : OK.
- `PP010` — Forme : OK.
- `PP011` — Forme : typographie (espaces autour de “:”, format international) à homogénéiser.
- `PP012` — Forme : OK; homogénéiser “E-mail/email”.
- `PP013` — Forme : OK.
- `PP014` — Fond : déclaration utile; vérifier exactitude du nom du congrès (“mutual French annual congress…” semble étrange) + cohérence des dates. Forme : phrase longue → scinder; “figures and tables has” → accord.

### Abstract

- `PP015` — Forme : OK.
- `PP016` (Aim) — Fond : objectif clair; préciser si l’étude est “strategy-level” (LHP vs OPA) plutôt que “laparoscopy vs open” (sinon confusion). Forme : OK.
- `PP017` (Methods) — Fond : bon résumé; préciser le cadre temporel (2010–2021) et le critère principal (24-month stoma-free survival) pour être complet. Stats : “PSM on preoperative variables” OK mais vague (citer SMD/caliper/ratio).
- `PP018` (Results) — Fond : trop chargé, et incohérent avec le corps :
  - effectifs `285` vs `284` et `OPA 147` vs `146`;
  - p-values discordantes (ex. morbidité globale) avec Table 3;
  - HR annoncé `2.28` alors que Table 4 donne `1.84` (même IC 95%) → à corriger impérativement.
  Forme : “fewer stoma reversal” → “lower stoma reversal rate”; clarifier “open anastomosis” → “open resection with primary anastomosis + diverting ileostomy”.
- `PP019` (Conclusion) — Fond : conclusion cohérente mais un peu catégorique (“open” comme recommandation) alors que le message clinique est surtout “anastomose protégée vs Hartmann”. Forme : OK.
- `PP020` (MeSH) — Forme : harmoniser majuscules/ponctuation; vérifier termes MeSH exacts (“Sigmoid Resection” vs “Sigmoidectomy”, etc.).

### Introduction

- `PP021` — Forme : OK.
- `PP022` — Fond : bon cadrage + guidelines. Forme : nombreuses micro-corrections :
  - espaces/collages de citations (`rates3,4`);
  - “unstable of very high-risk” → “unstable or very high-risk”;
  - “long term” → “long-term”;
  - phrase finale à alléger (trop d’informations en un seul paragraphe).
  Références : ensemble pertinent.
- `PP023` — Fond : logique (lavage → résection). Forme : fautes/anglicismes à corriger (“laparscopic”, “datas” → “data”). Cohérence : “Current data suggest…” est un peu général → préciser “selected patients / expert centers” + éviter de sur-vendre l’effet (hétérogénéité des séries).
- `PP024` — Fond : hypothèse intéressante (adhérences → rétablissement). Forme : “may reduces” → “may reduce”; préciser que l’argument est surtout théorique/observational.
- `PP025` — Fond : objectif clair; Forme : harmoniser capitalisation (“Open Sigmoid Resection…”) et préciser l’endpoint principal.

### Patients and Methods

- `PP026` — Forme : OK.
- `PP027` — Forme : OK.
- `PP028` — Fond : bonne description de la source; à compléter selon revue (approbation éthique/CPP, gestion RGPD, qualité/contrôle du registre). Cohérence : mention du NCT OK.
- `PP029` — Fond : inclusion claire; préciser la définition d’“emergency” et du “diverting stoma” (ileostomie systématique dans OPA ?).
- `PP030` — Fond : choix ASA4/ICU comme proxy d’instabilité est défendable mais doit être discuté (risque de mauvaise classification, sélection de patients plus “stables”). Forme : OK.
- `PP031` — Forme : OK.
- `PP032` — Forme : OK.
- `PP033` — Fond : définition correcte, mais point statistique important : le décès est un competing risk (pas seulement une censure “non-informative”) → à justifier ou compléter (analyse de sensibilité competing-risk). Clarifier “successful restoration” (endpoints exacts).
- `PP034` — Forme : OK.
- `PP035` — Fond : secondaires pertinents; préciser fenêtre de suivi pour hernie/récidive (et méthode de recueil) + définir “intraoperative morbidity”.
- `PP036` — Forme : OK.
- `PP037` — Fond : descriptions OK, mais :
  - la référence `25` (Hartmann 1921) ne décrit pas la laparoscopie → référence inadaptée ici;
  - phrase à corriger (“OPA group), Decisions…” → scinder);
  - préciser critères de conversion et si l’analyse est “intention-to-treat” (LHP incluant conversions).
- `PP038` — Forme : OK.
- `PP039` — Stats : globalement bon (covariables listées, SMD), mais plusieurs fragilités :
  - faute (“abdolute”);
  - “pairwise deletion” : risque de biais + incohérence d’effectifs selon variables → préférer imputation multiple ou au minimum tableau des manquants + sensibilité;
  - vous rapportez des médianes mais mentionnez “paired t-tests” (à harmoniser : soit moyenne±SD + t-test, soit médiane (IQR) + Wilcoxon);
  - préciser ratio (1:1), métrique de distance (logit PS), et si caliper utilisé.
  Présentation : après appariement, privilégier SMD plutôt que p-values dans Table 1.
- `PP040` — Stats : modèle Cox + variance robuste cluster sur paires = cohérent. À ajouter : vérification de l’hypothèse de proportionnalité, gestion competing-risk (au moins en discussion). La ref `27` parle du matching “with replacement” alors que vous faites “without replacement” → vérifier pertinence.
- `PP041` — Forme : OK; “RStudio” n’est pas l’environnement d’exécution statistique (plutôt R + packages) — détail mineur mais souvent attendu (version de R, packages `MatchIt/survival` etc.).

### Results

- `PP042` — Forme : OK.
- `PP043` — Forme : OK.
- `PP044` — Fond : bon flowchart, mais incohérences à corriger avec abstract et Table 1 (`n=284` ici vs `285` ailleurs; OPA 146 vs 147). Présentation : mentionner aussi le nombre de patients perdus de vue / censurés si disponible.
- `PP045` — Fond : OK; Forme : OK.
- `PP046` — Forme : contient des notes (“Flowchart + … Tableau 1”) à supprimer. Cohérence : si Table 1 est la baseline post-PSM, éviter d’insister sur des p-values.
- `PP047` — Forme : OK.
- `PP048` — Forme : parenthèse non fermée (“(PSM cohort,”). Fond : conversion très élevée (49%) → donner `n`/denominateur et renvoyer aux limites (raisons non capturées).
- `PP049` — Fond : intéressant (gestes différents). Références : `28` (ligature IMA en cancer rectal) est peu adaptée pour une définition technique → envisager une source plus pertinente. Forme : virgules/phrases à corriger (“(59%), Intraoperative…” → point).
- `PP050` — Forme : placeholder à supprimer.
- `PP051` — Forme : OK.
- `PP052` — Fond : OK; cohérence : p-values à aligner avec abstract. Présentation : si “non significatif”, donner aussi l’IC (au moins pour endpoints clés).
- `PP053` — Forme : “group/” + phrase incomplète; à réécrire. Fond : l’AL (8%) mérite un peu plus de granularité (Hinchey III/IV, timing, management) si disponible; sinon le dire.
- `PP054` — Forme : virgule finale; préciser définition/diagnostic d’hernie, durée de suivi réelle, et taux de données manquantes (ces événements tardifs sont sensibles à la perte de suivi).
- `PP055` — Forme : placeholder à supprimer.
- `PP056` — Forme : homogénéiser “Stoma-free survival” / “Stoma Free Survival”.
- `PP057` — Fond/Stats : OK (reverse KM, Cox robuste), mais :
  - référence de figure : le flowchart est Fig. 1, la courbe KM semble plutôt Fig. 2 → harmoniser (“Figure 1” apparaît 3 fois ici);
  - suivi médian très différent entre groupes (671 vs 390 j) → à expliquer et discuter (censure informative possible);
  - cohérence : HR ici = `1.84` (à aligner avec abstract et discussion).
- `PP058` — Stats : analyse de sous-groupes conversion intéressante, mais attention à l’interprétation (conversion = variable post-traitement, confounding by indication). À améliorer : donner les effectifs convertis/non-convertis post-PSM + éventuellement test d’interaction/approche multi-groupes.

### Discussion / conclusion

- `PP059` — Forme : OK.
- `PP060` — Fond : bon rappel objectif/comparateur; préciser explicitement que la question est la “stratégie” (Hartmann vs anastomose protégée) plus que l’accès chirurgical seul.
- `PP061` — Fond : synthèse claire. Prudence : “does not overcome the intrinsic limitations” est plausible mais un peu fort pour un rétrospectif (résiduel confounding + facteurs décisionnels de rétablissement non capturés).
- `PP062` — Références : pertinent (DIVERTI/LADIES). Cohérence : vous citez LADIES + DIVERTI; vous pouvez aussi citer l’essai de Oberkofler (`9`) si vous vous appuyez sur “randomized trials” au pluriel.
- `PP063` — Fond : justification du benchmark OK; à renforcer : expliquer pourquoi l’OPA est “open” (manque de puissance/variables pour laparoscopie?) pour éviter le procès “comparateur non symétrique”.
- `PP064` — Fond : argument conversion bien construit. Stats : dire explicitement que l’absence de différence converti vs non-converti peut aussi refléter un manque de puissance (IC large).
- `PP065` — Références : `29` est une opinion/lettre, donc faible niveau de preuve → à encadrer et/ou compléter. Forme : “reviews suggests” → accord; éviter de tirer des conclusions “emergency peritonitis” à partir de littérature surtout élective (`30–33`) sans nuance.
- `PP066` — Fond : bon point sur l’AL et la diversion. Références : `13` est un protocole; `34–35` ne sont pas centrées sur l’AL en péritonite. Pour soutenir “leak rates acceptable with diversion”, s’appuyer aussi sur RCT/meta-analyses (`8–12`) ou données observationnelles dédiées.
- `PP067` — Fond : positionnement bibliographique solide, avec une bonne lecture des limites des papiers cités. Forme : OK.
- `PP068` — Fond : implications claires; ton un peu prescriptif (“remains the reference”) → éventuellement reformuler en “should be considered the preferred strategy in stable/eligible patients”, en rappelant la nature observationnelle.
- `PP069` — Fond : bonnes forces/limites, mais j’ajouterais explicitement : competing risk, gestion des manquants, effet centre/volume, et “intention regarding reversal” (décisionnel majeur).
- `PP070` — Forme : conclusion OK. Cohérence : éviter “open” si le message est “anastomose protégée” (indépendamment de l’abord, selon expertise).
- `PP071` — Forme : note interne en français → à supprimer.
- `PP072` — Forme : note interne → à supprimer/intégrer; bonne idée de tableau comparatif LADIES/DIVERTI si la revue l’accepte.
- `PP073` — Forme : note interne → à intégrer dans le raisonnement (conversion vs stratégie).
- `PP074` — Forme : note interne → à intégrer; sur le fond, question pertinente mais à traiter prudemment (hors scope si non analysé).
- `PP075` — Forme : note interne → à reformuler en argument sourcé (et démontrer “largest series” avec référence/phrase factuelle).
- `PP076` — Forme : note interne → à intégrer; sur le fond cohérent avec guidelines.
- `PP077` — Forme : note interne → à intégrer; risque de redite avec conclusion.

### Déclarations (financement, conflits, remerciements)

- `PP078` — Forme : OK.
- `PP079` — Forme : OK.
- `PP080` — Forme : OK.
- `PP081` — Forme : “AM:” incompréhensible → à supprimer et compléter déclaration de conflits pour tous les auteurs.
- `PP082` — Forme : OK.
- `PP083` — Fond : OK; préciser la nature du support (financier ? logistique ? accès au registre ?) et s’il y a influence.

### Bibliographie (pertinence + forme)

- `PP084` — Forme : OK.
- `PP085` — Pertinence : référence historique (Hinchey) pertinente.
- `PP086` — Pertinence : revue utile (mise à jour).
- `PP087` — Pertinence : revue systématique utile.
- `PP088` — Pertinence : directement liée à Hartmann/reversal.
- `PP089` — Pertinence : guideline internationale clé.
- `PP090` — Pertinence : guideline ASCRS clé; Forme : idéalement donner la pagination complète (pas une seule page).
- `PP091` — Pertinence : utile (France) mais accessibilité internationale limitée; ajouter date d’accès et/ou citer une recommandation internationale en priorité selon revue.
- `PP092` — Pertinence : essai DIVERTI (central).
- `PP093` — Pertinence : essai randomisé (central).
- `PP094` — Pertinence : essai LADIES (central).
- `PP095` — Pertinence : méta-analyse RCT (utile).
- `PP096` — Pertinence : méta-analyse (utile).
- `PP097` — Pertinence : protocole DIVERTI2; à présenter comme “ongoing trial” (niveau de preuve limité).
- `PP098` — Pertinence : MI vs open en urgence; vérifier adéquation exacte à votre population (perforated diverticulitis / colectomy vs sigmoid).
- `PP099` — Pertinence : similaire; idem (définitions).
- `PP100` — Pertinence : SCANDIV (contexte lavage vs résection).
- `PP101` — Pertinence : suivi LOLA.
- `PP102` — Pertinence : revue lavage Hinchey III (OK).
- `PP103` — Pertinence : papier clé sur LHP vs open Hartmann.
- `PP104` — Pertinence : pertinent mais mélange stratégies; à bien cadrer (comme vous le faites en discussion).
- `PP105` — Pertinence : revue “emergency laparoscopic sigmoidectomy” (utile).
- `PP106` — Pertinence : série “totally laparoscopic 2-step” (utile mais sélection/expert center).
- `PP107` — Pertinence : reversal laparoscopique vs open (utile pour l’argument adhérences/reversal).
- `PP108` — Pertinence : standard Clavien–Dindo.
- `PP109` — Pertinence : historique; Forme : source web d’un PDF (souvent mal acceptée). Si vous gardez, sécuriser la citation (source bibliographique stable) ou supprimer si non nécessaire.
- `PP110` — Pertinence : méthode PSM (OK).
- `PP111` — Pertinence : variance en matching *with replacement* alors que vous faites *without replacement* → vérifier/adapter la référence.
- `PP112` — Pertinence : peu adaptée (cancer rectal) pour définir “high ligation” en diverticulite → remplacer.
- `PP113` — Pertinence : opinion/lettre; à utiliser avec prudence ou compléter par données.
- `PP114` — Pertinence : méta-analyse surtout contexte électif; à nuancer si utilisée pour l’urgence.
- `PP115` — Pertinence : urgence diverticulaire (OK).
- `PP116` — Pertinence : Cochrane (souvent électif) → prudence d’extrapolation.
- `PP117` — Pertinence : RCT Sigma (électif) → prudence d’extrapolation.
- `PP118` — Pertinence : patterns populationnels (utile en contexte).
- `PP119` — Pertinence : morbidité fermeture iléostomie (utile pour la discussion sur diversion).

### Tableaux / figures

- `PP120` — Forme : OK.
- `PP121` (Table 1) — Présentation à revoir : `n` incohérent, “Not concerned”, ligne “Actual diverticulitis” vide, mélange virgules/points décimaux, p-values post-PSM (préférer SMD), et harmoniser les définitions (ASA, immunosuppression, etc.).
- `PP122` (Table 2) — Forme : OK mais “Not concerned” → “N/A”; harmoniser décimales et étoiles de significativité.
- `PP123` (Table 3) — Problèmes : ligne “Anastomotic leakage” incomplète (valeurs LHP/p-value), “NS” à remplacer par des p-values ou expliciter “not estimable”; harmoniser les unités (“ICU hospitalization, %”).
- `PP124` (Table 4) — Forme : OK; cohérence : c’est la source de vérité pour l’HR (à reporter partout).
- `PP125` — Forme : double point final.
- `PP126` (Fig 1) — Forme : “patients inclusion” → “patient inclusion” / “patient inclusion flow chart”.
- `PP127` (Fig 2) — Cohérence : vérifier couleurs (rouge/bleu) vs figures réelles; éviter répétitions; préciser “stratified log-rank” OK.
- `PP128` — Forme : “Covariate Covariate” à corriger; cohérence couleurs (rouge/bleu) vs figure.
- `PP129` — Forme : OK.
- `PP130` — Cohérence : mentionner clairement la méthode (Cox avec SE robustes) et vérifier couleurs (orange/pink/blue) vs figure.


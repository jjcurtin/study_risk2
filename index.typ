// Simple numbering for non-book documents
#let equation-numbering = "(1)"
#let callout-numbering = "1"
#let subfloat-numbering(n-super, subfloat-idx) = {
  numbering("1a", n-super, subfloat-idx)
}

// Theorem configuration for theorion
// Simple numbering for non-book documents (no heading inheritance)
#let theorem-inherited-levels = 0

// Theorem numbering format (can be overridden by extensions for appendix support)
// This function returns the numbering pattern to use
#let theorem-numbering(loc) = "1.1"

// Default theorem render function
#let theorem-render(prefix: none, title: "", full-title: auto, body) = {
  if full-title != "" and full-title != auto and full-title != none {
    strong[#full-title.]
    h(0.5em)
  }
  body
}
// Some definitions presupposed by pandoc's typst output.
#let content-to-string(content) = {
  if content.has("text") {
    content.text
  } else if content.has("children") {
    content.children.map(content-to-string).join("")
  } else if content.has("body") {
    content-to-string(content.body)
  } else if content == [ ] {
    " "
  }
}

#let horizontalrule = line(start: (25%,0%), end: (75%,0%))

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms.item: it => block(breakable: false)[
  #text(weight: "bold")[#it.term]
  #block(inset: (left: 1.5em, top: -0.4em))[#it.description]
]

// Some quarto-specific definitions.

#show raw.where(block: true): set block(
    fill: luma(230),
    width: 100%,
    inset: 8pt,
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let fields = old_block.fields()
  let _ = fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.abs
  }
  block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == str {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == content {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

// Subfloats
// This is a technique that we adapted from https://github.com/tingerrr/subpar/
#let quartosubfloatcounter = counter("quartosubfloatcounter")

#let quarto_super(
  kind: str,
  caption: none,
  label: none,
  supplement: str,
  position: none,
  subcapnumbering: "(a)",
  body,
) = {
  context {
    let figcounter = counter(figure.where(kind: kind))
    let n-super = figcounter.get().first() + 1
    set figure.caption(position: position)
    [#figure(
      kind: kind,
      supplement: supplement,
      caption: caption,
      {
        show figure.where(kind: kind): set figure(numbering: _ => {
          let subfloat-idx = quartosubfloatcounter.get().first() + 1
          subfloat-numbering(n-super, subfloat-idx)
        })
        show figure.where(kind: kind): set figure.caption(position: position)

        show figure: it => {
          let num = numbering(subcapnumbering, n-super, quartosubfloatcounter.get().first() + 1)
          show figure.caption: it => block({
            num.slice(2) // I don't understand why the numbering contains output that it really shouldn't, but this fixes it shrug?
            [ ]
            it.body
          })

          quartosubfloatcounter.step()
          it
          counter(figure.where(kind: it.kind)).update(n => n - 1)
        }

        quartosubfloatcounter.update(0)
        body
      }
    )#label]
  }
}

// callout rendering
// this is a figure show rule because callouts are crossreferenceable
#show figure: it => {
  if type(it.kind) != str {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let children = old_title_block.body.body.children
  let old_title = if children.len() == 1 {
    children.at(0)  // no icon: title at index 0
  } else {
    children.at(1)  // with icon: title at index 1
  }

  // TODO use custom separator if available
  // Use the figure's counter display which handles chapter-based numbering
  // (when numbering is a function that includes the heading counter)
  let callout_num = it.counter.display(it.numbering)
  let new_title = if empty(old_title) {
    [#kind #callout_num]
  } else {
    [#kind #callout_num: #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block,
    block_with_new_content(
      old_title_block.body,
      if children.len() == 1 {
        new_title  // no icon: just the title
      } else {
        children.at(0) + new_title  // with icon: preserve icon block + new title
      }))

  align(left, block_with_new_content(old_callout,
    block(below: 0pt, new_title_block) +
    old_callout.body.children.at(1)))
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black, body_background_color: white) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color,
        width: 100%,
        inset: 8pt)[#if icon != none [#text(icon_color, weight: 900)[#icon] ]#title]) +
      if(body != []){
        block(
          inset: 1pt, 
          width: 100%, 
          block(fill: body_background_color, width: 100%, inset: 8pt, body))
      }
    )
}



// document mode
#let doc(
  title: none,
  running-head: none,
  authors: none,
  affiliations: none,
  authornote: none,
  abstract: none,
  keywords: none,
  margin: (x: 2.5cm, y: 2.5cm),
  paper: "us-letter",
  font: ("New Computer Modern"),
  fontsize: 11pt,
  leading: 0.55em,
  spacing: 0.55em,
  first-line-indent: 1.25cm,
  toc: false,
  cols: 1,
  doc,
) = {

  set page(
    paper: paper,
    margin: margin,
    header-ascent: 50%,
    header: locate(
        loc => if [#loc.page()] == [1] {
          []
        } else {
          grid(
            columns: (1fr, 1fr),
            align(left)[#running-head],
            align(right)[#counter(page).display()]
          )
        }
    ),
  )
  
  set par(
    justify: true, 
    leading: leading,
    first-line-indent: first-line-indent
  )

  // Also "leading" space between paragraphs
  show par: set block(spacing: spacing)

  set text(
    font: font,
    size: fontsize
  )

  if title != none {
    align(center)[
      #v(6em)#block(below: leading*4)[
        #text(size: fontsize*1.4)[#title]
      ]
    ]
  }
  
  if authornote != none {
    footnote(numbering: "*", authornote)
    counter(footnote).update(0)
  }
  
  if authors != none {
    align(center)[
      #block(below: leading*2)[
        #set text(size: fontsize*1.15)
        // Formatting depends on N authors 1, 2, or 2+
        #if authors.len() > 2 {
          for a in authors [
            #a.name#super[#a.affiliations]#if a!=authors.at(authors.len()-1) [#if a==authors.at(authors.len()-2) [, and] else [,]]
          ]
        } 
        #if authors.len() == 2 {
          for a in authors [
            #a.name#super[#a.affiliations]#if a!=authors.at(authors.len()-1) [and]
          ]
        }
        #if authors.len() == 1 {
          for a in authors [
            #a.name#super[#a.affiliations]
          ]
        }
      ]
    ]
  }

  if affiliations != none {
    align(center)[
      #block(below: leading*2)[
        #for a in affiliations [
          #super[#a.id]#a.name \
        ]
      ]
    ]
  }

  if abstract != none {
    block(inset: (x: 10%, y: 0%), below: 3em)[
      #align(center, text("Abstract"))
      #set par(first-line-indent: 0pt, leading: leading)
      #abstract
      #if keywords != none {[
        #v(1em)#text(weight: "regular", style: "italic")[Keywords:] #h(0.25em) #keywords
      ]}
    ]
  }

  /* Redefine headings up to level 5 */
  show heading.where(
    level: 1
  ): it => block(width: 100%, below: leading*2, above: leading*2)[
    #set align(center)
    #set text(size: fontsize)
    #it.body
  ]

  show heading.where(
    level: 2
  ): it => block(width: 100%, below: leading*2, above: leading*2)[
    #set align(left)
    #set text(size: fontsize)
    #it.body
  ]

  show heading.where(
    level: 3
  ): it => block(width: 100%, below: leading*2, above: leading*2)[
    #set align(left)
    #set text(size: fontsize, style: "italic")
    #it.body
  ]

  show heading.where(
    level: 4
  ): it => text(
    size: 1em,
    weight: "bold",
    it.body + [.]
  )

  show heading.where(
    level: 5
  ): it => text(
    size: 1em,
    weight: "bold",
    style: "italic",
    it.body + [.]
  )

  if cols == 1 {
    doc
  } else {
    columns(cols, gutter: 4%, doc)
  }
  
}

// Manuscript mode
#let man(
  title: none,
  running-head: none,
  authors: none,
  affiliations: none,
  authornote: none,
  abstract: none,
  keywords: none,
  margin: (x: 2.5cm, y: 2.5cm),
  paper: "us-letter",
  font: ("Times New Roman"),
  fontsize: 12pt,
  leading: 2em,
  spacing: 2em,
  first-line-indent: 1.25cm,
  toc: false,
  cols: 1,
  doc,
) = {

  set page(
    paper: paper,
    margin: margin,
    header-ascent: 50%,
    header: grid(
      columns: (1fr, 1fr),
      align(left)[#running-head],
      align(right)[#counter(page).display()]
    )
  )
  
  set par(
    justify: false, 
    leading: leading,
    first-line-indent: first-line-indent
  )

  // Also "leading" space between paragraphs
  show par: set block(spacing: spacing)

  set text(
    font: font,
    size: fontsize
  )

  if title != none {
    align(center)[
      #v(8em)#block(below: leading*2)[
        #text(weight: "bold", size: fontsize)[#title]
      ]
    ]
  }
  
  if authornote != none {
    footnote(numbering: "*", authornote)
    counter(footnote).update(0)
  }
  
  if authors != none {
    align(center)[
      #block(above: leading, below: leading)[
        // Formatting depends on N authors 1, 2, or 2+
        #if authors.len() > 2 {
          for a in authors [
            #a.name#super[#a.affiliations]#if a!=authors.at(authors.len()-1) [#if a==authors.at(authors.len()-2) [, and] else [,]]
          ]
        } 
        #if authors.len() == 2 {
          for a in authors [
            #a.name#super[#a.affiliations]#if a!=authors.at(authors.len()-1) [and]
          ]
        }
        #if authors.len() == 1 {
          for a in authors [
            #a.name#super[#a.affiliations]
          ]
        }
      ]
      #counter(footnote).update(0)
    ]
  }
  
  if affiliations != none {
    align(center)[
      #block(above: leading, below: leading)[
        #for a in affiliations [
          #super[#a.id]#a.name \
        ]
      ]
    ]
  }

  pagebreak()
  
  if abstract != none {
    block(above: 0em, below: 2em)[
      #align(center, text(weight: "bold", "Abstract"))
      #set par(first-line-indent: 0pt, leading: leading)
      #abstract
      #if keywords != none {[
        #text(weight: "regular", style: "italic")[Keywords:] #h(0.25em) #keywords
      ]}
    ]
  }
  pagebreak()

  /* Redefine headings up to level 5 */
  show heading.where(
    level: 1
  ): it => block(width: 100%, below: leading, above: leading)[
    #set align(center)
    #set text(size: fontsize)
    #it.body
  ]

  show heading.where(
    level: 2
  ): it => block(width: 100%, below: leading, above: leading)[
    #set align(left)
    #set text(size: fontsize)
    #it.body
  ]

  show heading.where(
    level: 3
  ): it => block(width: 100%, below: leading, above: leading)[
    #set align(left)
    #set text(size: fontsize, style: "italic")
    #it.body
  ]

  show heading.where(
    level: 4
  ): it => text(
    size: 1em,
    weight: "bold",
    it.body + [.]
  )

  show heading.where(
    level: 5
  ): it => text(
    size: 1em,
    weight: "bold",
    style: "italic",
    it.body + [.]
  )

  if cols == 1 {
    doc
  } else {
    columns(cols, gutter: 4%, doc)
  }
  
}

// Journal mode
#let jou(
  title: none,
  running-head: none,
  authors: none,
  affiliations: none,
  authornote: none,
  abstract: none,
  keywords: none,
  margin: (x: 2.5cm, y: 2.5cm),
  paper: "us-letter",
  font: ("Times New Roman"),
  fontsize: 10pt,
  leading: 0.5em, // Space between lines
  spacing: 0.5em, // Space between paragraphs
  first-line-indent: 0cm,
  toc: false,
  cols: 2,
  mode: none,
  doc,
) = {

  set page(
    paper: paper,
    margin: margin,
    header-ascent: 50%,
    header: locate(
        loc => if [#loc.page()] == [1] {
          []
        } else {
          grid(
            columns: (1fr, 1fr),
            align(left)[#running-head],
            align(right)[#counter(page).display()]
          )
        }
    ),
  )
  
  set par(
    justify: true, 
    leading: leading,
    first-line-indent: first-line-indent
  )

  // Also "leading" space between paragraphs
  show par: set block(spacing: spacing)

  set text(
    font: font,
    size: fontsize
  )

  if title != none {
    align(center)[
      #v(3em)#block(below: leading*4)[
        #text(size: fontsize*1.8)[#title]
      ]
    ]
  }
  
  if authornote != none {
    footnote(numbering: "*", authornote)
    counter(footnote).update(0)
  }
  
  if authors != none {
    align(center)[
      #block(below: leading*2)[
        #set text(size: fontsize*1.3)
        // Formatting depends on N authors 1, 2, or 2+
        #if authors.len() > 2 {
          for a in authors [
            #a.name#super[#a.affiliations]#if a!=authors.at(authors.len()-1) [#if a==authors.at(authors.len()-2) [, and] else [,]]
          ]
        } 
        #if authors.len() == 2 {
          for a in authors [
            #a.name#super[#a.affiliations]#if a!=authors.at(authors.len()-1) [and]
          ]
        }
        #if authors.len() == 1 {
          for a in authors [
            #a.name#super[#a.affiliations]
          ]
        }
      ]
      #counter(footnote).update(0)
    ]
  }
  
  if affiliations != none {
    align(center)[
      #block(below: leading*2)[
        #for a in affiliations [
          #super[#a.id]#a.name \
        ]
      ]
    ]
  }

  if abstract != none {
    block(inset: (x: 15%, y: 0%), below: 3em)[
      #set text(size: 9pt)
      #set par(first-line-indent: 0pt, leading: leading)
      #abstract
      #if keywords != none {[
        #v(1em)#text(weight: "regular", style: "italic")[Keywords:] #h(0.25em) #keywords
      ]}
    ]
  }

  /* Redefine headings up to level 5 */
  show heading.where(
    level: 1
  ): it => block(width: 100%, below: leading*2, above: leading*2)[
    #set align(center)
    #set text(size: fontsize)
    #it.body
  ]

  show heading.where(
    level: 2
  ): it => block(width: 100%, below: leading*2, above: leading*2)[
    #set align(left)
    #set text(size: fontsize)
    #it.body
  ]

  show heading.where(
    level: 3
  ): it => block(width: 100%, below: leading*2, above: leading*2)[
    #set align(left)
    #set text(size: fontsize, style: "italic")
    #it.body
  ]

  show heading.where(
    level: 4
  ): it => text(
    size: 1em,
    weight: "bold",
    it.body + [.]
  )

  show heading.where(
    level: 5
  ): it => text(
    size: 1em,
    weight: "bold",
    style: "italic",
    it.body + [.]
  )

  if cols == 1 {
    doc
  } else {
    columns(cols, gutter: 4%, doc)
  }
  
}
#let brand-color = (:)
#let brand-color-background = (:)
#let brand-logo = (:)

#set page(
  paper: "us-letter",
  margin: (x: 1.25in, y: 1.25in),
  numbering: "1",
  columns: 1,
)

#show: document => man(
  title: "Using smartphone sensing and machine learning for personalized daily lapse risk prediction in a national sample of people with opioid use disorder",
  authors: (
    (
      name: "Kendra Wyant",
      affiliations: "aff-1",
      email: []
    ),
    (
      name: "John J. Curtin",
      affiliations: "aff-1",
      email: [jjcurtin\@wisc.edu]
    ),
    
  ),
  affiliations: (
    (
      id: "aff-1",
      name: "Department of Psychology, University of Wisconsin-Madison"
    ),
    
  ),
  abstract: [Enter abstract here.

],
  keywords: [Substance use disordersPrecision mental health],
  document,
)

= Introduction
<introduction>
About 4.5 million adults in the United States met criteria for opioid use disorder (OUD) in 2024 \(Substance Abuse and Mental Health Services Administration, n.d.). OUD is associated with increased rates of mortality and morbidity, including infectious disease, psychiatric comorbidity, and disability \(Hser et al., 2017). The societal burden of untreated OUD is substantial. The combined economic costs of lost productivity, health care expenditures, criminal justice involvement, loss of life, and reduced quality of life have been estimated to exceed \$1 trillion \(Florence et al., 2021).

Medications for OUD (MOUD), including methadone and buprenorphine, are effective at reducing opioid use, craving, and overdose risk \(Ma et al., 2019). They are also associated with improvements in mental health and social functioning, particularly when maintained over extended periods. However, despite strong evidence for MOUD, access remains limited.

Even among individuals retained in treatment, lapses are frequent and confer a critical period of risk for treatment dropout, relapse, and overdose \(Heinz et al., 2025; Weinstein et al., 2017). Opioid lapses are particularly high-risk events. Reduced opioid tolerance (e.g., after a period of abstinence) and the increasing prevalence of fentanyl and other synthetic opioids in the drug supply substantially increase the likelihood of fatal overdose.

Identifying vulnerable periods of high lapse risk and contextualizing that risk (i.e., #emph[Why] risk is high) could enable individuals to take proactive action to prevent lapse. However, the risk factors that precipitate lapses are highly individualized, numerous, dynamic, and interactive. The optimal supports to address these risks vary both across individuals and within individuals over time. Thus, effective interventions must be personalized to both the individual and the moment to meet the complex and changing needs of people in recovery.

Advances in smartphone sensing methods, including brief daily self-report surveys and passive geolocation sensing, offer a powerful way to collect densely sampled risk-relevant data \(Mohr et al., 2017). These data can be used as inputs into machine learning models to generate temporally precise predictions of lapse risk \(Heinz et al., 2025; Wyant et al., 2024). Methods from interpretable machine learning can be used to identify personalized predictors of risk at any moment in time \(Molnar, 2022).

Daily self-report surveys can reliably collect proximal risk factors for lapse, such as recent craving, abstinence self-efficacy, risky situations, and substance use behaviors. Research suggests that individuals can adhere to self-report sensing methods while using substances \(Jones et al., 2019; Wyant et al., 2023). However, it is less clear to what extent people in recovery from illicit substance use disorders can, or are willing to, report risk information accurately enough to develop prediction models. People with substance use disorders (SUD) may experience greater instability in their day-to-day lives (e.g., stigma or legal consequences may make access to healthcare, stable housing, or supportive relationships more difficult). This instability could make it difficult to recall and report recent behaviors and events promptly or accurately. It may also skew baseline perceptions of what constitutes a risky or stressful experience.

Supplementing self-report data with passively sensed geolocation data could make up for imprecise reports of risk factors or be used for lapse prediction during periods of non-adherence to self-report surveys. Moreover, geolocation data could provide a complementary and distinct set of intervenable features. For example, geolocation data may provide insight into time spent in risky locations or changes in routine (e.g., job loss) that may indicate life stressors. These features may become even more powerful when combined with contextual information from public sources (e.g., census data, alcohol outlet density) or from self-report (e.g., self-evaluated riskiness of specific locations). Geolocation data have been demonstrated to contain sufficient signal for predicting craving \(Epstein et al., 2020), a putative precursor to opioid lapse, and therefore represent a promising sensing method for predicting lapse risk.

In the current study, we developed a machine learning model that predicts next-day opioid lapse risk using daily self-report and geolocation sensing data. Our study has several notable strengths. First, we recruited a national sample of participants across the United States who were diverse in regard to race and ethnicity, income, gender, sexual orientation, and geographic location. Second, participants provided sensing data and opioid lapse reports for up to one year, a duration that exceeds previously published SUD prediction studies and allowed us to capture longer-term recovery trajectories. Third, our sample consisted of patients in early recovery from OUD receiving MOUD, ensuring that the study population closely reflected the intended clinical audience for our model. Fourth, we explicitly evaluated model performance across demographic subgroups to assess algorithmic fairness. Finally, we identified the most influential predictors using two complementary quantitative approaches, providing insight into the features that drive lapse risk predictions.

= Methods
<methods>
== Transparency and Openness
<transparency-and-openness>
We adhere to research transparency principles that are crucial for robust and replicable science. First, we published this study's protocol as a registered report (International Registered Report Identifier \[IRRID\]: DERR1-10.2196/29563) during the initial enrollment of pilot participants \(Moshontz et al., 2021). Second, we followed the Transparent Reporting of a multivariable prediction model for Individual Prognosis Or Diagnosis extension for Artificial Intelligence (TRIPOD+AI) guidelines \(Collins et al., 2024). A TRIPOD+AI checklist is available in the supplement. Finally, our features, labels, questionnaires, and other study materials are publicly available on our OSF page (#link("https://osf.io/zvm7s/overview")) and our annotated analysis scripts and results are publicly available on our study website (#link("https://jjcurtin.github.io/study_risk2/")).

== Participants
<participants>
We recruited participants in early recovery from OUD from across the United States. We recruited through national digital advertising and collaborations with treatment providers at MOUD clinics. Our recruitment strategy was designed to create a diverse sample with respect to demographics (gender, age, race, and ethnicity), and geographic location (urban and rural). We required participants:

- were age 18 or older,
- could read, write, and speak in English,
- were enrolled in and adherent with an MOUD program for at least 1 month but no longer than 12 months or enrolled in or recently completed a day treatment program, and
- had an Android smartphone with an active data plan.

We did not exclude participants for comorbid substance use or other psychiatric disorders.

== Procedure
<procedure>
Participants completed three video or phone visits over approximately 12 months. During the enrollment visit, study staff obtained written informed consent and collected demographic information. They walked participants through how to download the Smart Technology for Addiction Recovery (STAR) study app, provided a set of video tutorials to learn how to use the app, and instructed participants to complete the intake survey within the app. The STAR app was developed by the UW Madison Center for Health Enhancement Systems Studies and used for all data collection. Within the app participants could control their data sharing options, monitor completed study tasks, receive reminder notifications about tasks, message staff, and access STAR's suite of resources and tools for people in recovery from AUD \(Gustafson et al., 2014). Enrolled participants met with study staff one week later to troubleshoot technical issues. At the end of the study enrollment period participants met briefly with study staff for a debriefing session. While on study, participants were expected to complete daily surveys, monthly surveys, and share geolocation sensing data. Other sensing data streams (i.e., daily video check-ins, cellular communications, and app usage data) were collected as part of the parent grant's aims (NIDA R01 DA047315). All procedures were approved by the University of Wisconsin-Madison Institutional Review Board (Study \#2019-0656).

Participants were compensated \$40 for completing the enrollment visit and watching the STAR video tutorials and \$10 for the two follow-up phone visits. Participants were compensated \$10 per month for the monthly survey, up to \$15 per month for the daily surveys#footnote[\$0.25 per survey plus an additional \$3.70 for completing at least 12 surveys in the first half of the month and an additional \$3.70 for completing at least 12 surveys in the second half of the month (total range of \$14.40 - \$15.15)], and \$5 per month for sharing geolocation data.#footnote[Participants were also compensated for providing other sensing data as part of the parent grant's aims, including \$10 per month for sharing cellular communication logs and text message content and up to \$10 per month for the daily video check-in.] Participants were also paid \$50 per month to offset the cost of maintaining a cellphone data plan.

== Measures
<measures>
=== Individual Differences
<individual-differences>
We collected self-report information about demographics (age, gender, sexual orientation, race, ethnicity, education, employment, and income). Zip codes from participants' reported home addresses were linked to Rural--Urban Commuting Area (RUCA) codes to characterize the rural--urban status of their residences \(Economic Research Service US Department of Agriculture, n.d.). These variables were collected to characterize our sample. They were also included as features in our model to allow for potential interactions with the sensing features and control for demographic differences on lapse risk. Demographic variables with some evidence for influencing OUD treatment access and clinical outcomes and/or carrying general societal stigma (gender, race/ethnicity, income, sexual orientation, and geographic location) were used to perform subgroup analyses for evaluating model fairness \(Greenfield et al., 2007; Kilaru et al., 2020; Lofaro et al., 2025; Martin et al., 2022; Olfson et al., 2022; Pinedo, 2019; Xin et al., 2023).

We collected information about OUD history to characterize OUD severity and recovery stability across our sample. These measures included self-reported Diagnostic and Statistical Manual of Mental Disorders, Fifth Edition OUD symptoms \(American Psychiatric Association, 2022), past month opioid use, past month residential treatment for OUD, past month receipt of psychiatric medication, preferred opioid, preferred route of administration, and lifetime history of overdose. As part of the aims of the parent project, we collected many other trait and state measures throughout the study. A complete list of all measures can be found in our registered report \(Moshontz et al., 2021).

=== Daily Survey
<daily-survey>
Participants completed a brief (1-2 minute) 16-item daily survey each day on study through the STAR app. The daily survey became available in the app at 5:00 AM CST each morning and participants had 24 hours to complete it. Participants could enable push notifications for reminder prompts to complete the survey. Participants reported opioid lapses on the first daily survey item. If participants responded "yes" to the question "Have you used any opioids for non-medical reasons that you have not yet reported?", they were prompted to select the day(s) and time(s) of the lapse(s). Times were reported in 6-hour increments (12:00am--5:59am, 6:00am--11:59am, 12:00pm--5:59pm, 6:00pm--11:59pm). These reports served as the primary outcome for the lapse risk prediction model. On the remaining 15 items, participants reported any other drugs that they had used, whether they took their MOUD as prescribed in the past 24 hours, and rated the maximum intensity of recent (i.e., in the past 24 hours) experiences of pain, craving, risky situations, stressful events, and pleasant events. They also rated their sleep and how depressed, angry, anxious, relaxed, and happy they have felt in the past 24 hours. Lastly, participants responded to 2 future-facing items that asked about participants' motivation and confidence to continue to avoid using opioids for non-medical reasons over the next week. The full daily survey is available in the supplement.

=== Geolocation Sensing
<geolocation-sensing>
The STAR app passively recorded participants' time-stamped geolocations (i.e., latitude and longitude) every 1.5-15 minutes, depending on their movement. We augmented the geolocation data with self-reported subjective context. On each monthly survey participants were asked 6 questions about frequently visited locations (i.e., locations that the participant spent more than 3 minutes on 2 or more times in a month) from the previous month. Participants were asked to describe the type of place, what they typically do there, the general frequency of pleasant and unpleasant experiences at the location, and the extent to which spending time there helps or harms their recovery. The full set of location questions is avilable in the supplement.

== Data Analytic Strategy
<data-analytic-strategy>
Data preprocessing, modeling, and Bayesian analyses were done in R using the tidymodels ecosystem \(Goodrich et al., 2023; Kuhn, 2022; Kuhn & Wickham, 2020). Models were trained and evaluated using high-throughput computing resources provided by the University of Wisconsin Center for High Throughput Computing \(Center for High Throughput Computing, 2006).

=== Feature Engineering
<feature-engineering>
Features were calculated using only data collected before the start of each prediction window to ensure our models were making true future predictions. We calculated a total of 297 features from three data sources:

+ #emph[The prediction window day.] We created one-hot coded features for day of the week for the start of the prediction window.

+ #emph[Demographic individual differences.] For demographics, we created ordinal features for age, education, and income based on predefined response ranges (see #ref(<tbl-1>, supplement: [Table]) for ranges), an ordinal feature for RUCA code associated with home address (range 1-10), and one-hot coded features for gender (male, not male), race and ethnicity (non-Hispanic White, Hispanic and/or not White), orientation (heterosexual, not heterosexual), and employment (employed, not employed/looking for work, not employed/not looking or able to work).

+ #emph[Dynamic daily survey and geolocation sensing data.] For both sets of sensing features we calculated two types of features: raw and difference features. Raw features represent the feature value calculated within a given scoring epoch (e.g., the maximum urge rating reported on the daily survey during the 168 hours immediately preceding the start of the prediction window). Difference features capture participant-level changes from their baseline scores. Specifically, we subtracted each participant's mean score for each feature using all available data prior to the prediction window from the associated raw feature (e.g., the participant's average urge rating across all prior daily surveys subtracted from the maximum urge rating in the preceding 168 hours).

  For the daily survey, we calculated raw and difference features for the most recent and the min, max, and median responses in the 168 hours preceeding the start of the prediciton window for the 13 5-point likert scale items (items 4-16; pain, urge, risky situation, stressful event, pleasant event, sleep, depressed, angry, anxious, relaxed, happy, abstinence motivation, and abstinence confidence). We also calculated raw and difference rate features based on counts of previous opioid lapses, other drug use, missed MOUD doses (items 1-3), and completed daily surveys in 24 and 168-hour feature scoring epochs (e.g., counts of opioid lapses within the 168 hours preceding the start of a prediction window divided by 168).

  We used two feature scoring epochs (24 and 168 hours before the start of the prediction window) to create proximal and distal geolocation features. Raw geolocation points were cross-checked against known locations with reported subjective context. We used a threshold of 50 meters for matching context to geolocation points. We calculated raw and difference features for sum duration of time out of the home in the evenings (i.e., not at their home between the hours of 7:00pm-4:00am). We calculated raw and difference features for sum duration of time spent at locations according to what they indicated they do at the location (spend time with friends, socialize with new people, religious activities, relax, spend time with family, volunteer, receive mental health care, receive physical health care, receive MOUD treatment, drink alcohol, take classes, work), how pleasant and unpleasant their experiences typically are at the location (Always, Most of the time, Sometimes, Rarely, Never), and how helpful and harmful the location is to their recovery (Extremely, Considerably, Moderately, Mildly, Not at all). We also calculated a raw and difference feature of location variance (i.e., the extent to which a participant's location changes over a feature scoring epoch).

All features (sets 1-3 above) were included in our full model. We also fit three models with varying ablated sensing features from set 3. Specifically, these models ablated daily survey features, ablated geolocation features, and ablated daily survey and geolocation features combined to assess the incremental predictive value of these features.

Other feature engineering steps performed during cross-validation included imputing missing values (median imputation for numeric features and mode imputation for nominal features), dummy coding nominal features, normalizing features to have a mean of zero and standard deviation of 1, bringing outlying values (|z-score| \> 5) to the fence, and removing zero and near-zero variance features as determined from held-in data. We selected coarse median/mode methods for handling missing data due to the low rates of missing values and computational costs associated with more advanced forms of imputation (e.g., KNN imputation, multiple imputation). A sample feature engineering script (i.e., tidymodels recipe) containing all feature engineering steps is available on our OSF study page.

=== Lapse Labels
<lapse-labels>
Prediction windows started at 6:00am in participants' local timezones and ended at 5:59am the next day. This window start time was selected to more closely approximate a typical wake-sleep cycle as opposed to midnight-to-midnight calendar day. For each participant the first prediction window began at 6:00am on their second day of participation and rolled forward day-by-day until their participation ended (i.e., the last prediction window ended at 5:59am on the day of their last recorded daily survey). Responses from the first daily survey item were used to label each prediction window as lapse or no lapse. For example, if a participant reported a lapse on December 12 from 12:00am-5:59am the prediction window spanning 6:00am on December 11 through 5:59am on December 12 was labeled as a lapse.

=== Cross-Validation
<cross-validation>
We used nested cross-validation for selection and evaluation of the final candidate model configurations. We used 1 repeat of 5-fold cross-validation on the inner loop for model selection and 6 repeats of 5-fold cross-validation on the outer loop for evaluation. Participants were grouped so that all of their data were always in the held-in or held-out fold to avoid bias introduced when predicting a participant's data from their own data. Folds were stratified so that all folds contained comparable proportions of individuals who lapsed while on study.

=== Model Configurations
<model-configurations>
We initially considered three statistical algorithms that differed in terms of assumptions and bias-variance tradeoff: elastic net, random forest, and XGBoost. Preliminary analyses using simple 6x5 cross-validation suggested that XGBoost was superior (see supplement for initial performance estimates by algorithm). Additionally, we planned to use the Shapley additive explanations (SHAP) method for calculating feature importance which is optimized for XGBoost. Therefore we only considered XGBoost for the final nested cross-validation analyses.

Final candidate model configurations differed across sensible values for key hyperparameters and outcome resampling method (i.e., up-sampling and down-sampling of the outcome at ratios ranging from 5:1 to 1:1). All resampling was exclusively done in the held-in training data (i.e., held-out data were not resampled) to prevent biasing performance estimates \(Vandewiele et al., 2021). All model configurations were fit for the full model and the three feature ablation models (ablated daily survey features, ablated geolocation features, and ablated daily survey and geolocation features combined).

=== Model Evaluation
<model-evaluation>
We evaluated the best full model's probability predictions across three domains: discrimination, calibration, and overall performance. We follow recommendations for reporting measures and plots to characterize these performance domains \(Van Calster et al., 2025).

==== auROC and Model Comparisons
<auroc-and-model-comparisons>
Our performance metric for model selection and evaluation was the area under the receiver operating characteristic curve (auROC). auROC is an aggregate measure of discrimination across all possible decision thresholds. Specifically, it represents the probability that the model will assign a higher predicted probability to a randomly selected positive case (lapse) compared to a randomly selected negative case (no lapse).

We used a Bayesian hierarchical generalized linear model to estimate the posterior distribution and 95% credible intervals (CI) for auROC for the 30 held-out test sets (i.e., held-out data in the outer loop of the nested cross-validation procedure) for our best full and ablated models. We used weakly informative, data-dependent priors to regularize and reduce overfitting. Priors were set as follows: residual standard deviation \~ normal(location=0, scale=exp(2)), intercept (after centering predictors) \~ normal(location=2.3, scale=1.3), the two coefficients for window width contrasts \~ normal (location=0, scale=2.69), and covariance \~ decov(regularization=1, concentration=1, shape=1, scale=1). We set two random intercepts to account for our resampling method: one for the repeat, and another for the fold nested within the repeat. We specified three model contrasts that compared the full model to each of the three ablated models (full vs.~ablated daily survey features, full vs.~ablated geolocation features, full vs.~ablated daily survey and geolocation features combined). auROCs were transformed using the logit function and regressed as a function of model contrast. From the Bayesian model we obtained the posterior distribution for auROC for the full and baseline models. We reported the median posterior probability for auROC and 95% CIs for each model. We then conducted four Bayesian model comparisons to determine the probability that the full model differed systematically in performance compared to the three ablated models.

We performed five dichotomous subgroup analyses to assess the fairness of our model's predictions. Using the same 30 held-out test sets and the same modeling procedure as above, we calculated the median posterior probability and 95% CI for auROC for each model separately by gender (not male vs.~male), race/ethnicity (Hispanic and/or non-White vs.~non-Hispanic White), income (less than \$25,000 vs.~more than \$25,000), sexual orientation (heterosexual vs.~not heterosexual), and geographic location (rural vs.~urban)^\[We followed guidelines from the United States Health Resources and Services Administration and define urban as an area where the primary commuting flow is within a metropolitan core of 50,000 or more people (RUCA code = 1) and rural as anything not urban (RUCA codes 2-10). \(Health Resources and Services Administration, n.d.). We conducted Bayesian group comparisons to assess the likelihood that each model performs differently by group.

==== Calibration and Overall Performance
<calibration-and-overall-performance>
To further evaluate our model's predictions, we used our inner resampling procedure (1 repeat of 5-fold cross validation) on the full data set to select and fit a single best model configuration for the full model. The final configuration selected represents the most reliable and robust configuration for deployment.

Calibration is an indicator of how well a model's predicted probabilities match the true observed outcomes. For example, a well-calibrated model that assigns a 30% lapse risk prediction should observe lapses in approximately 30% of such cases. We used Platt scaling to calibrate our final full model's raw probabilities from the 5 held-out folds \(Platt, 1999). We provided a calibration plot of these raw and calibrated probabilities. To characterize overall performance we reported Brier scores for the raw and calibrated probabilities. Brier scores are the mean squared difference between the predicted probabilities and observed outcome and range from 0 (perfect accuracy) to 1 (perfect inaccuracy). We also provided histograms of risk probability distributions by true lapse outcome.

=== Feature Importance
<feature-importance>
Feature importance values provide insight into the features that have the most influence on the model's predictions. For every prediction, we can extract feature importance values providing actionable insight into intervenable targets for lapse risk (i.e., for a specific individal at a specific moment). We used the same single 5-fold cross-validation procedure (see Calibration and Overall Performance section) to calculate raw Shapley values for each observation in held-out data \(Lundberg & Lee, 2017). The magnitude of the raw Shapley value indicates how much the feature score for that observation adjusted the prediction (in log-odds units) relative to the mean prediction across all observations. Positive Shapley values indicate that the feature score increased the prediction for that observation and negative values indicate that the feature score decreased the prediction. In other words, higher Shapley values suggest the feature increases lapse risk and lower values suggest the feature decreases lapse risk. Shapley values are inherently additive. For any observation, Shapley values can be summed to create a total adjustment score for the predicted value. We created feature categories by collapsing features that differed only by scoring epoch and calculation into a single feature category (e.g., Raw features for most recent urge rating, and min, max, and mean urge rating in the 168 hours preceding the start of the prediction window were combined to form a "Urge, Raw" feature category). Raw and difference features were not combined as their relationship to risk were not uniform. We plotted individual Shapley values and feature categories as partial dependence plots to illustrate these feature-risk relationships.

Feature importance values can also be aggregated across all participants and all observations to provide a relative rank ordering of the most important features. We calculated overall feature importance in two ways. First, we used a traditional approach in which we calculated the mean absolute Shapley value for each feature category across all observations. This approach summarizes overall feature importance by averaging the magnitude of each feature's contribution. However, it can be skewed toward features that exhibit infrequent but very large Shapley values, potentially overstating the importance of features that are strongly associated with the outcome, but only come up in a small subset of observations. The second way we calculated feature importance was by calculating the proportion of observations in which each feature category had the highest Shapley value. This approach summarizes how frequently a feature category is influential across observations (i.e., considering both magnitude and prevalence). We provided a plot of the relative ranking of feature categories by their overall feature importance using these two methods.

= Results
<results>
== Participants
<participants-1>
We enrolled 330 participants across 47 states in the United States from April 2021 through December 2024. To be considered enrolled in the study, participants were required to be eligible, consent, and complete the first personalized monthly survey where geolocation context were provided.#footnote[405 participants consented to participate in the study and provided at least one daily survey; however, 75 participants dropped out prior to completing the first personalized monthly survey and were therefore not considered fully enrolled in the study.] Of these 330, 14 were excluded for not providing at least 28 days of EMA data with acceptable adherence#footnote[If adherence fell below 33% (roughly 9 surveys in a 28-day period) for a prolonged period of time, participant study end dates were changed to match the start of this adherence drop.], 7 participants were excluded due to evidence of careless responding on daily surveys, and 13 participants were excluded due to having insufficient context data for geolocation points (fewer than two contextualized locations). Our final analysis sample consisted of 296 participants. Participant demographic and OUD characteristics of our final sample is presented in #ref(<tbl-1>, supplement: [Table]).

#block[
#figure([
#{set text(font: ("Arial Narrow", "Source Sans Pro", "sans-serif")); table(
  columns: 3,
  align: (left,right,right,),
  table.header(table.cell(align: left)[], table.cell(align: right)[N], table.cell(align: right)[%],),
  table.hline(),
  table.cell(align: left)[Age], table.cell(align: right)[], table.cell(align: right)[],
  table.cell(colspan: 3, stroke: (bottom: (paint: black, thickness: 2.25pt)))[#strong[]],
  table.cell(align: left)[22-25], table.cell(align: right)[7], table.cell(align: right)[2.4],
  table.cell(align: left)[26-35], table.cell(align: right)[105], table.cell(align: right)[35.5],
  table.cell(align: left)[36-45], table.cell(align: right)[107], table.cell(align: right)[36.1],
  table.cell(align: left)[46-55], table.cell(align: right)[60], table.cell(align: right)[20.3],
  table.cell(align: left)[56-65], table.cell(align: right)[13], table.cell(align: right)[4.4],
  table.cell(align: left)[Over 65], table.cell(align: right)[4], table.cell(align: right)[1.4],
  table.cell(align: left)[Gender], table.cell(align: right)[], table.cell(align: right)[],
  table.cell(colspan: 3, stroke: (bottom: (paint: black, thickness: 2.25pt)))[#strong[]],
  table.cell(align: left)[Man], table.cell(align: right)[158], table.cell(align: right)[53.4],
  table.cell(align: left)[Woman], table.cell(align: right)[131], table.cell(align: right)[44.3],
  table.cell(align: left)[Non-binary], table.cell(align: right)[4], table.cell(align: right)[1.4],
  table.cell(align: left)[Not listed above], table.cell(align: right)[2], table.cell(align: right)[0.7],
  table.cell(align: left)[Orientation], table.cell(align: right)[], table.cell(align: right)[],
  table.cell(colspan: 3, stroke: (bottom: (paint: black, thickness: 2.25pt)))[#strong[]],
  table.cell(align: left)[Straight, that is, not gay or lesbian], table.cell(align: right)[237], table.cell(align: right)[80.1],
  table.cell(align: left)[Lesbian or gay], table.cell(align: right)[17], table.cell(align: right)[5.7],
  table.cell(align: left)[Bisexual], table.cell(align: right)[35], table.cell(align: right)[11.8],
  table.cell(align: left)[Not sure], table.cell(align: right)[1], table.cell(align: right)[0.3],
  table.cell(align: left)[Not listed above], table.cell(align: right)[4], table.cell(align: right)[1.4],
  table.cell(align: left)[Race and Ethnicity (select all that apply)], table.cell(align: right)[], table.cell(align: right)[],
  table.cell(colspan: 3, stroke: (bottom: (paint: black, thickness: 2.25pt)))[#strong[]],
  table.cell(align: left)[American Indian/Alaskan Native], table.cell(align: right)[17], table.cell(align: right)[5.7],
  table.cell(align: left)[Asian], table.cell(align: right)[3], table.cell(align: right)[1.0],
  table.cell(align: left)[Black/African American], table.cell(align: right)[43], table.cell(align: right)[14.5],
  table.cell(align: left)[Native Hawaiian/Other Pacific Islander], table.cell(align: right)[3], table.cell(align: right)[1.0],
  table.cell(align: left)[White/Caucasian], table.cell(align: right)[235], table.cell(align: right)[79.4],
  table.cell(align: left)[Hispanic, Latino, or Spanish origin], table.cell(align: right)[26], table.cell(align: right)[8.8],
  table.cell(align: left)[Not listed above], table.cell(align: right)[5], table.cell(align: right)[1.7],
  table.cell(align: left)[Geographic Location], table.cell(align: right)[], table.cell(align: right)[],
  table.cell(colspan: 3, stroke: (bottom: (paint: black, thickness: 2.25pt)))[#strong[]],
  table.cell(align: left)[rural], table.cell(align: right)[57], table.cell(align: right)[19.3],
  table.cell(align: left)[urban], table.cell(align: right)[239], table.cell(align: right)[80.7],
  table.cell(align: left)[Education], table.cell(align: right)[], table.cell(align: right)[],
  table.cell(colspan: 3, stroke: (bottom: (paint: black, thickness: 2.25pt)))[#strong[]],
  table.cell(align: left)[8th grade or less], table.cell(align: right)[5], table.cell(align: right)[1.7],
  table.cell(align: left)[Some high school, but did not graduate], table.cell(align: right)[25], table.cell(align: right)[8.4],
  table.cell(align: left)[High school graduate or GED], table.cell(align: right)[92], table.cell(align: right)[31.1],
  table.cell(align: left)[Some college or 2-year degree], table.cell(align: right)[130], table.cell(align: right)[43.9],
  table.cell(align: left)[4-year college graduate], table.cell(align: right)[30], table.cell(align: right)[10.1],
  table.cell(align: left)[More than 4-year or advanced degree], table.cell(align: right)[12], table.cell(align: right)[4.1],
  table.cell(align: left)[Employment], table.cell(align: right)[], table.cell(align: right)[],
  table.cell(colspan: 3, stroke: (bottom: (paint: black, thickness: 2.25pt)))[#strong[]],
  table.cell(align: left)[Disabled, not able to work], table.cell(align: right)[34], table.cell(align: right)[11.5],
  table.cell(align: left)[Employed, working 1-39 hours per week], table.cell(align: right)[69], table.cell(align: right)[23.3],
  table.cell(align: left)[Employed, working 40 or more hours per week], table.cell(align: right)[54], table.cell(align: right)[18.2],
  table.cell(align: left)[Not employed, NOT looking for work], table.cell(align: right)[32], table.cell(align: right)[10.8],
  table.cell(align: left)[Not employed, looking for work], table.cell(align: right)[102], table.cell(align: right)[34.5],
  table.cell(align: left)[Retired], table.cell(align: right)[3], table.cell(align: right)[1.0],
  table.cell(align: left)[Household Income], table.cell(align: right)[], table.cell(align: right)[],
  table.cell(colspan: 3, stroke: (bottom: (paint: black, thickness: 2.25pt)))[#strong[]],
  table.cell(align: left)[Less than \$25,000], table.cell(align: right)[164], table.cell(align: right)[55.4],
  table.cell(align: left)[\$25,000 - \$34,999], table.cell(align: right)[42], table.cell(align: right)[14.2],
  table.cell(align: left)[\$35,000 - \$49,999], table.cell(align: right)[38], table.cell(align: right)[12.8],
  table.cell(align: left)[\$50,000 - \$74,999], table.cell(align: right)[29], table.cell(align: right)[9.8],
  table.cell(align: left)[\$75,000 - \$99,999], table.cell(align: right)[13], table.cell(align: right)[4.4],
  table.cell(align: left)[\$100,000 - \$149,999], table.cell(align: right)[3], table.cell(align: right)[1.0],
  table.cell(align: left)[\$150,000 - \$199,999], table.cell(align: right)[4], table.cell(align: right)[1.4],
  table.cell(align: left)[\$200,000 or more], table.cell(align: right)[1], table.cell(align: right)[0.3],
  table.cell(align: left)[Self-reported DSM-5 OUD Symptom Count], table.cell(align: right)[], table.cell(align: right)[],
  table.cell(colspan: 3, stroke: (bottom: (paint: black, thickness: 2.25pt)))[#strong[]],
  table.cell(align: left)[Under threshold (0-1)], table.cell(align: right)[5], table.cell(align: right)[1.7],
  table.cell(align: left)[Mild (2-3)], table.cell(align: right)[0], table.cell(align: right)[0.0],
  table.cell(align: left)[Moderate (4-5)], table.cell(align: right)[5], table.cell(align: right)[1.7],
  table.cell(align: left)[Severe (6+)], table.cell(align: right)[284], table.cell(align: right)[95.9],
  table.cell(align: left)[Past Month Opioid Use], table.cell(align: right)[], table.cell(align: right)[],
  table.cell(colspan: 3, stroke: (bottom: (paint: black, thickness: 2.25pt)))[#strong[]],
  table.cell(align: left)[No], table.cell(align: right)[201], table.cell(align: right)[67.9],
  table.cell(align: left)[Yes], table.cell(align: right)[93], table.cell(align: right)[31.4],
  table.cell(align: left)[Past Month Detox or Residential Treatment], table.cell(align: right)[], table.cell(align: right)[],
  table.cell(colspan: 3, stroke: (bottom: (paint: black, thickness: 2.25pt)))[#strong[]],
  table.cell(align: left)[No], table.cell(align: right)[189], table.cell(align: right)[63.9],
  table.cell(align: left)[Yes], table.cell(align: right)[106], table.cell(align: right)[35.8],
  table.cell(align: left)[Past Month Psychiatric Medication], table.cell(align: right)[], table.cell(align: right)[],
  table.cell(colspan: 3, stroke: (bottom: (paint: black, thickness: 2.25pt)))[#strong[]],
  table.cell(align: left)[No], table.cell(align: right)[151], table.cell(align: right)[51.0],
  table.cell(align: left)[Yes], table.cell(align: right)[144], table.cell(align: right)[48.6],
  table.cell(align: left)[Preferred Opioid], table.cell(align: right)[], table.cell(align: right)[],
  table.cell(colspan: 3, stroke: (bottom: (paint: black, thickness: 2.25pt)))[#strong[]],
  table.cell(align: left)[Fentanyl], table.cell(align: right)[50], table.cell(align: right)[16.9],
  table.cell(align: left)[Heroin], table.cell(align: right)[122], table.cell(align: right)[41.2],
  table.cell(align: left)[Prescription opioid not for opioid treatment], table.cell(align: right)[93], table.cell(align: right)[31.4],
  table.cell(align: left)[Medication for opioid treatment], table.cell(align: right)[28], table.cell(align: right)[9.5],
  table.cell(align: left)[Preferred Route of Administration], table.cell(align: right)[], table.cell(align: right)[],
  table.cell(colspan: 3, stroke: (bottom: (paint: black, thickness: 2.25pt)))[#strong[]],
  table.cell(align: left)[Injection], table.cell(align: right)[104], table.cell(align: right)[35.1],
  table.cell(align: left)[Oral], table.cell(align: right)[56], table.cell(align: right)[18.9],
  table.cell(align: left)[Smoke], table.cell(align: right)[48], table.cell(align: right)[16.2],
  table.cell(align: left)[Sniff or snort], table.cell(align: right)[83], table.cell(align: right)[28.0],
  table.cell(align: left)[Other], table.cell(align: right)[3], table.cell(align: right)[1.0],
  table.cell(align: left)[Lifetime History of Overdose], table.cell(align: right)[], table.cell(align: right)[],
  table.cell(colspan: 3, stroke: (bottom: (paint: black, thickness: 2.25pt)))[#strong[]],
  table.cell(align: left)[Never], table.cell(align: right)[119], table.cell(align: right)[40.2],
  table.cell(align: left)[1 time], table.cell(align: right)[41], table.cell(align: right)[13.9],
  table.cell(align: left)[2-3 times], table.cell(align: right)[69], table.cell(align: right)[23.3],
  table.cell(align: left)[4-5 times], table.cell(align: right)[26], table.cell(align: right)[8.8],
  table.cell(align: left)[More than 5 times], table.cell(align: right)[39], table.cell(align: right)[13.2],
  table.hline(),
  table.footer([#text(style: "italic")[Note: ]], [], [],
    [#super[] N = 296], [], [],),
)}
], caption: figure.caption(
position: top, 
[
Demographic and Clinical Characteristics
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-1>


]
== Adherence, Features, and Labels
<adherence-features-and-labels>
Mean days on study across participants was 278.32 days (range 30-392 days). 77.7% of participants (230/296) remained on study for at least six months. Daily survey adherence was high. On average participants completed 77.8% of the daily survey prompts (range 40-100%). Participants provided, on average, 263.50 daily geolocation points (range 2.17-871.34).

The average percentage of missing values across daily survey features was low (mean=0.71%, range 0-1.10%). The average percentage of missing values across geolocation features was higher (mean=12.0%, range 0-15.6%). Across participants we generated a total of 82,156 day-level labels. Thirty-six percent of participants (106/296) reported an opioid lapse while on study (mean=4.93, range 0-117). This resulted in 1.8% of the labels positive for lapse (1,458/82,156 labels). We stratified the data on a variable of whether someone lapsed on study to ensure our imbalanced outcome was evenly split (and roughly equivalent to the sample prevalance) across cross-validation folds.

== auROC and Model Comparisons
<auroc-and-model-comparisons-1>
The median posterior probability for the best full model was 0.94, with narrow 95% CI (\[0.93, 0.95\]). To evaluate the incremental predictive value of our dynamic sensing features, we compared the performance of our full model to the performance of three ablated models: 0.63 (ablated daily survey features), 0.94 (ablated geolocation features), and 0.55 (ablated daily survey and geolocation features). There was strong evidence that the full model's performance was better compared to models that ablated the daily survey features and the daily survey and geolocation features combined (probabilities = 1.00). There was no evidence that the full model performed better than a model with geolocation features ablated (probability = 0.34). The median difference in auROC, 95% Bayesian CI, and posterior probability that that the auROC difference was greater than 0 for all ablation contrasts are presented in #ref(<tbl-2>, supplement: [Table]).

#block[
#figure([
#{set text(font: ("Arial Narrow", "Source Sans Pro", "sans-serif")); table(
  columns: 4,
  align: (left,left,left,left,),
  table.header(table.cell(align: left)[Contrast], table.cell(align: left)[Median auROC Difference], table.cell(align: left)[Bayesian CI], table.cell(align: left)[Probability],),
  table.hline(),
  table.cell(align: left)[full vs. ablated daily survey], table.cell(align: left)[0.314], table.cell(align: left)[\[0.29, 0.338\]], table.cell(align: left)[1.000],
  table.cell(align: left)[full vs. ablated geolocation], table.cell(align: left)[-0.002], table.cell(align: left)[\[-0.009, 0.005\]], table.cell(align: left)[0.342],
  table.cell(align: left)[full vs. ablated daily survey and geolocation], table.cell(align: left)[0.389], table.cell(align: left)[\[0.363, 0.414\]], table.cell(align: left)[1.000],
  table.hline(),
  table.footer([#super[] Median auROC differences greater than 0 indicate the full model, on average, performed better than the ablated model (i.e., full vs. ablated daily survey, full vs. ablated geolocation, full vs. ablated daily survey and geolocation). Bayesian CI represents the range of values where there is a 95% probability that the true auROC difference lies within that range. Probability indicates the posterior probability that this difference is greater than 0 (i.e., the full model is performing better).], [], [], [],),
)}
], caption: figure.caption(
position: top, 
[
Median difference in auROC, 95% Bayesian credible interval (CI), and posterior probability that that the auROC difference was larger than 0 for all ablated model contrasts.
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-2>


]
Our fairness subgroup comparisons for the full model revealed strong evidence that performance differed by gender, income, race and ethnicity, sexual orientation, and geographic location (probabilities \> 0.97). Notably, our model performed better for individuals with an annual income of less than \$25,000 compared to individuals with an annual income greater than \$25,000, thus favoring the disadvantaged group. While differences in performance estimates exist, this model is likely still clinically meaningful across subgroups, as all subgroups yielded median auROCs between 0.91 - 0.95 (#ref(<fig-1>, supplement: [Figure])). A table of fairness subgroup comparisons is available in the supplement.

#block[
#figure([
#box(image("index_files/figure-typst/notebooks-mak_figures-fig-1-output-2.png"))
], caption: figure.caption(
position: bottom, 
[
Posterior probabilities for area under the receiver operating curve (auROC) by demographic subgroup. auROC ranges from .5 (chance performance) to 1 (perfect performance). Subgroups advantaged in access to substance use treatment and outcomes (male, non-Hispanic White, greater than \$25,000 annual income, urban location, and heterosexual) are depicted in dark purple. Subgroups disadvantaged in access to substance use treatment and outcomes (not male, Hispanic and/or not White, less than \$25,000 annual income, rural location, and not heterosexual) are depicted in green. Overall model performance across groups is depicted as the dashed grey line.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-1>


]
== Calibration and Overall Performance
<calibration-and-overall-performance-1>
The best full model configuration selected with 1 repeat of 5-fold cross-validation used an xgboost statistical algorithm and up-sampled the minority class.#footnote[The best model configuration used 1:5 upsampling of the majority class and the following hyperparameter values: learning rate = .1, tree depth = 3, mtry = 50.] This model produced generally well-calibrated raw probabilities (brier score = 0.014; left panel of #ref(<fig-2>, supplement: [Figure])). We attempted to improve calibration with Platt scaling and results were comparable (brier score = 0.014). Histograms of raw risk probability distributions separately by true lapse outcome are presented in the right panel of #ref(<fig-2>, supplement: [Figure]).

#block[
#figure([
#box(image("index_files/figure-typst/notebooks-mak_figures-fig-2-output-1.png"))
], caption: figure.caption(
position: bottom, 
[
The top panel presents a calibration plot of raw and Platt calibrated risk probabilities. Predicted probabilities (x-axis) are binned into deciles. Observed lapse probability (y-axis) represents the proportion of actual lapses observed in each bin. The dashed diagonal represents perfect calibration. Points below the line indicate overestimation and points above the line indicate underestimation. Raw probabilities are depicted as the dark purple line. Platt calibrated probabilities are depicted as the green dashed line. The rug plots along the x-axes depict the observation frequency in each bin for the raw (bottom) and Platt calibrated (top) probabilities. The bottom left and right panel presents histograms of raw (uncalibrated) and Platt calibrated risk probability distributions, respectively, separately by true lapse outcome.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-2>


]
== Feature Importance
<feature-importance-1>
We aggregated individual Shapley values across all participants and all observations to provide a relative rank ordering of the most important features. A traditional mean absolute Shapley value approach revealed the most important feature (i.e., largest magnitude of change in log-odds) was differences in past opioid use. These feature was nearly two-fold more important than the next most important feature, raw urge ratings. #ref(<fig-3>, supplement: [Figure]) displays the top 30 (out of 186) feature categories, as defined by the 30 largest absolute mean shapley values. In the supplement, we provide full figures of all possible feature categories.

A second approach where we calculated the proportion of observations in which each feature category had the highest Shapley value (i.e., largest magnitude and prevalence), revealed the most important feature to be raw urge ratings, followed by raw abstinence confidence ratings. Differences in past opioid use was the third most important feature by this method, suggesting that while it has a strong relationship with lapse risk, it is not as prevalent across observations as other features. Notably, in this approach several additional comparably important features emerged, including differences in past pleasant experiences, differences in past stimulant use, and differences in time spent in helpful locations.

#block[
#figure([
#box(image("index_files/figure-typst/notebooks-mak_figures-fig-3-output-1.png"))
], caption: figure.caption(
separator: "", 
position: bottom, 
[
#block[
]
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-3>


]
To understand the direction in which these feature categories adjusted the lapse risk predictions, we plotted Shapley-feature value partial dependence plots for the top 30 feature categories (#ref(<fig-4>, supplement: [Figure])). Differenes in past opioid use lowered lapse risk probability for participants who did not lapse on study (i.e., a feature score of 0). Lapse probability dramatically increased when participants had increased reported opioid use over the past 24 and 168 hours. Unsurprisingly, raw urge and abstinence confidence ratings had linear relationships to lapse risk, with higher urge ratings increasing lapse risk probability and higher abstinence confidence decreasing lapse risk probability. There was a more nuanced finding for stimulant use and opioid lapse risk probability. Raw rate counts of stimulant use increased lapse risk; however, participants were at highest risk when recent stimulant use decreased (i.e., Stimulant Use, Diff).

#block[
#figure([
#box(image("index_files/figure-typst/notebooks-mak_figures-fig-4-output-1.png"))
], caption: figure.caption(
position: bottom, 
[
Feature importance partial dependence plots.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-4>


]
= Discussion
<discussion>
Our full model demonstrated that it is possible to future next-day lapses back to opioid use with high accuracy using smartphone sensing data (auROC = 0.94). Model predictions were well-calibrated, with estimated probabilities closely aligning with observed lapse rates. This is exciting given that our goal was to build a model that could provide insight into lapse risk.

Ablation model comparisons revealed that daily surveys alone captured nearly all of this predictive signal (auROC = 0.94). To our knowledge, no other published SUD study has collected daily survey data from participants continuously over the course of one year. Survey adherence rates in our sample were comparable to those observed in other clinical populations, suggesting that self‑report surveys are a feasible and risk-relevant sensing method for long‑term monitoring in this population.

When daily surveys were ablated from the model, predictive performance dropped substantially (auROC = 0.63). Although this performance remained above chance and exceeded that of a demographics‑only model (with both geolocation and daily surveys ablated; auROC = 0.55), it indicates that geolocation features captured only modest predictive signal. Moreover, given that ablating geolocation features did not result in any drop in performance, this signal likely overlaps with information already captured with the daily surveys. It is possible that the relatively low predictive value of geolocation sensing in this study was due to methodological limitations.

Although geolocation data can, in principle, be collected continuously moment‑by‑moment, in practice these data are difficult to collect reliably. In our study, geolocation features had substantially higher rates of missingness compared to daily surveys. Consequently, approximately 12% of geolocation features were imputed using median values derived from other individuals in the training dataset. Median values are by definition near the center of the feature distribution. These values are common and have limited influence on model predictions (i.e., low leverage). This was a purposeful decision in our design to ensure that missing data would not systematically influence predictions in either direction. However, this conservative approach likely contributed to reduced predictive performance of geolocation features, as imputations that minimize influence also limit the ability of those features to contribute risk‑relevant signal. Lastly, these median imputations are also not person-specific and as a result may not accurately represent typical values for the individual whose data are being imputed. As geolocation sensing methods become more robust and missingness is reduced, these features may become more powerful for lapse prediction.

A second limitation of our sensing method was that we relied on known locations with self-reported context. While this method provides meaningful contextual information, it is limited in its ability to capture the full range of locations participants visit (without substantially increasing participant burden). Passive methods for contextualizing location could address this limitation by providing risk-relevant contextual information across all locations. For example, latitude and longitude coordinates could be contextualized using publicly available datasets, such as alcohol outlet density, area deprivation index, and Narcotics Annonymous meeting times and locations. Increasing the number of locations would allow for potentially more variance in feature distributions within and between individuals.

To summarize, if the goal is prediction, geolocation sensing features are not necessary as they do not offer any unique incremental predictive value. However, if the goal is to have a more nuanced understanding of lapse risk across a more diverse array of risk features than geolocation is helpful. Adding geolocation features did not hurt model performance, and more data sources provides more opportunities for identifying intervenable targets specific to a person and moment in time.

We were excited to see that fairness discrepancies in model performance were minimal. In all subgroups, our model had a median auROC above 0.90, which is generally considered to be a benchmark of excellent performance. Nevertheless, differences of 0.02 - 0.03 (favoring the advantaged group) were present in race and ethnicity, sexual orientation, and geographic location comparisons. These group comparisons were the most imbalanced, with 19-29% of participants in the disadvantaged groups. Our model may perform better for these groups with greater representation in the training data. Future research into identifying additional risk factors for lapse that are more prevalent in these groups may also help to reduce these discrepancies.

Age may be another important dimension for evaluating model fairness. Older adults (65+ years) are less likely to receive substance use disorder treatment and often report lower levels of social interaction and support than younger adults. They may also be less comfortable with, or less able to provide, smartphone sensing data at the level of adherence needed for accurate prediction. We did not explicitly evaluate model fairness for this subgroup because we did not have the representation in our sample. We encourage researchers to perform this analysis in future studies.

When using a standard mean absolute Shapley value approach to estimate average feature importance across observations, the most important feature was prior opioid use. This finding is consistent with the relapse‑prevention literature, which identifies lapse as an important precursor to relapse, and prior prediction research demonstrating that lapses predict future lapses \(Wyant et al., 2024). Notably, this feature had a mean absolute Shapley value that was nearly two-fold higher than the next most important feature. When considering this model in the context of a recovery monitoring support system that provides daily insight into lapse risk, this finding may be misleading. Fewer than 2% of day-level observations contained a lapse, and the majority of participants did not lapse at all. Among those who did lapse, many only lapsed one time. Given that lapse cannot be used as a predictor until after it has occurred, in most cases other features may represent more clinically meaningful targets for intervention.

A more informative approach to characterize feature importance is to consider the proportion of observations in which a given feature had the highest Shapley value. This approach considers both the magnitude of a feature's effect (in terms of log-odds change in the prediction) and its prevalence across observations. Using this approach, urge ratings and abstinence confidence emerged as the features most frequently influencing lapse risk predictions. This suggests that individuals may benefit more often from recovery recommendations focussed on reducing urges (e.g., urge surfing) and increasing abstinence confidence (e.g., reflecting on recent successes). Additionally, this approach showed that while some features have low average importance (across all individuals and observations), they can be highly important on certain days for certain individuals (e.g., self-reports of anger).

Across both feature importance approaches, geolocation features emerged among the top subset of features. Specifically, location variance, time spent in helpful and harmful locations, and time spent at locations where religious activities are performed were among the top ten features. These findings suggest that although geolocation features may not provide unique signal for prediction, it may be worthwhile to still include them in the model to expand opportunities for intervention and provide precision-oriented recovery support.

When considering how our model might generalize in real world context, we must consider that participants were compensated for completing daily surveys and providing geolocation data, which may have influenced their willingness to provide data. However, participants received no additional direct benefit from their participation beyond financial compensation. In real‑world implementations, individuals may be more motivated to share data when it enables personalized recovery support. This perceived benefit may outweigh the burden of data collection, possibly even more so than financial incentive.

Additionally, we implemented daily survey compliance thresholds to exclude participants and adjust study end dates. This criterion was important because we generated our lapse labels from the daily survey data and thus we required consistent collection of this data stream for accurate observation labels. As a result, our estimates of adherence and model performance may be slightly optimistic. Still, even with this criterion, nearly 78% of participants remained in the study for at least six months, suggesting that long-term monitoring with daily surveys is feasible for a substantial proportion of individuals in this population.

Lastly, participants provided data for up to 12 months. This extended window of recovery is critical for evaluating the value of an algorithm intended for ongoing continuing care support and understanding how lapse risk evolves as people progress in their recovery. Unfortunately, our ability to address explanatory questions about the time course of lapse risk, and how individuals might cluster on different recovery trajectories is limited with traditional machine learning methods. These methods do not capture the repeated nature of sensing data. Each lapse prediction is treated as a new independent observation. We can account for the repeated observations in our sensing data by engineering features that capture individual changes over time to produce unbiased and precise estimates of predictive performance. However, more traditional time series models may be better suited for understanding the temporal dynamics of lapse risk over long periods of recovery.

In conclusion, it is possible to predict future lapses back to opioid use using smartphone sensing data. Daily surveys and geolocation sensing provide diverse coverage of important risk factors for lapse. Daily recovery recommendations can be mapped onto these factors to deliver support that is personalized to both the person and the moment. While this model was evaluated in a research setting, our use of a national clinical sample of individuals with OUD suggests it represents a promising step toward future real‑world implementation and evaluation.

= References
<references>
#block[
#block[
American Psychiatric Association. (2022). #emph[Diagnostic and statistical manual of mental disorders : DSM-5-TR]. 5th edition, text revision. Washington, DC : American Psychiatric Association Publishing, 2022.

] <ref-americanpsychiatricassociationDiagnosticStatisticalManual2022>
#block[
Center for High Throughput Computing. (2006). #emph[Center for high throughput computing]. Center for High Throughput Computing. #link("https://doi.org/10.21231/GNT1-HW21")

] <ref-chtc>
#block[
Collins, G. S., Moons, K. G. M., Dhiman, P., Riley, R. D., Beam, A. L., Van Calster, B., Ghassemi, M., Liu, X., Reitsma, J. B., Van Smeden, M., Boulesteix, A.-L., Camaradou, J. C., Celi, L. A., Denaxas, S., Denniston, A. K., Glocker, B., Golub, R. M., Harvey, H., Heinze, G., … Logullo, P. (2024). TRIPOD+AI statement: Updated guidance for reporting clinical prediction models that use regression or machine learning methods. #emph[BMJ], #emph[385], e078378. #link("https://doi.org/10.1136/bmj-2023-078378")

] <ref-collinsTRIPODAIStatementUpdated2024>
#block[
Economic Research Service US Department of Agriculture. (n.d.). #emph[Rural-Urban Commuting Area Codes - Users' Guide]. https:\/\/www.ers.usda.gov/data-products/rural-urban-commuting-area-codes/users-guide.

] <ref-economicresearchserviceusdepartmentofagricultureRuralUrbanCommutingArea>
#block[
Epstein, D. H., Tyburski, M., Kowalczyk, W. J., Burgess-Hull, A. J., Phillips, K. A., Curtis, B. L., & Preston, K. L. (2020). Prediction of stress and drug craving ninety minutes in the future with passively collected GPS data. #emph[Npj Digital Medicine], #emph[3]\(1), 26. #link("https://doi.org/10/ghqvcw")

] <ref-epsteinPredictionStressDrug2020>
#block[
Florence, C., Luo, F., & Rice, K. (2021). The economic burden of opioid use disorder and fatal opioid overdose in the United States, 2017. #emph[Drug and Alcohol Dependence], #emph[218], 108350. #link("https://doi.org/10.1016/j.drugalcdep.2020.108350")

] <ref-florenceEconomicBurdenOpioid2021>
#block[
Goodrich, B., Gabry, J., Ali, I., & Brilleman, S. (2023). #emph[Rstanarm: Bayesian Applied Regression Modeling via Stan].

] <ref-goodrichRstanarmBayesianApplied2023>
#block[
Greenfield, S. F., Brooks, A. J., Gordon, S. M., Green, C. A., Kropp, F., McHugh, R. K., Lincoln, M., Hien, D., & Miele, G. M. (2007). Substance abuse treatment entry, retention, and outcome in women: A review of the literature. #emph[Drug and Alcohol Dependence], #emph[86]\(1), 1--21. #link("https://doi.org/10.1016/j.drugalcdep.2006.05.012")

] <ref-greenfieldSubstanceAbuseTreatment2007>
#block[
Gustafson, D. H., McTavish, F. M., Chih, M.-Y., Atwood, A. K., Johnson, R. A., Boyle, M. G., Levy, M. S., Driscoll, H., Chisholm, S. M., Dillenburg, L., Isham, A., & Shah, D. (2014). A smartphone application to support recovery from alcoholism: A randomized clinical trial. #emph[JAMA Psychiatry], #emph[71]\(5), 566--572. #link("https://doi.org/10.1001/jamapsychiatry.2013.4642")

] <ref-gustafsonSmartphoneApplicationSupport2014>
#block[
Health Resources and Services Administration. (n.d.). #emph[How We Define Rural]. https:\/\/www.hrsa.gov/rural-health/about-us/what-is-rural.

] <ref-healthresourcesandservicesadministrationHowWeDefine>
#block[
Heinz, M. V., Price, G. D., Singh, A., Bhattacharya, S., Chen, C.-H., Asyyed, A., Does, M. B., Hassanpour, S., Hichborn, E., Kotz, D., Lambert-Harris, C. A., Li, Z., McLeman, B., Mishra, V., Stanger, C., Subramaniam, G., Wu, W., Campbell, C. I., Marsch, L. A., & Jacobson, N. C. (2025). A longitudinal observational study with ecological momentary assessment and deep learning to predict non-prescribed opioid use, treatment retention, and medication nonadherence among persons receiving medication treatment for opioid use disorder. #emph[Journal of Substance Use & Addiction Treatment], #emph[173]. #link("https://doi.org/10.1016/j.josat.2025.209685")

] <ref-heinzLongitudinalObservationalStudy2025>
#block[
Hser, Y.-I., Mooney, L. J., Saxon, A. J., Miotto, K., Bell, D. S., Zhu, Y., Liang, D., & Huang, D. (2017). High Mortality Among Patients With Opioid Use Disorder in a Large Healthcare System. #emph[Journal of Addiction Medicine], #emph[11]\(4), 315. #link("https://doi.org/10.1097/ADM.0000000000000312")

] <ref-hserHighMortalityPatients2017>
#block[
Jones, A., Remmerswaal, D., Verveer, I., Robinson, E., Franken, I. H. A., Wen, C. K. F., & Field, M. (2019). Compliance with ecological momentary assessment protocols in substance users: A meta-analysis. #emph[Addiction (Abingdon, England)], #emph[114]\(4), 609--619. #link("https://doi.org/10/gfsjzg")

] <ref-jonesComplianceEcologicalMomentary2019>
#block[
Kilaru, A. S., Xiong, A., Lowenstein, M., Meisel, Z. F., Perrone, J., Khatri, U., Mitra, N., & Delgado, M. K. (2020). Incidence of Treatment for Opioid Use Disorder Following Nonfatal Overdose in Commercially Insured Patients. #emph[JAMA Network Open], #emph[3]\(5), e205852. #link("https://doi.org/10.1001/jamanetworkopen.2020.5852")

] <ref-kilaruIncidenceTreatmentOpioid2020>
#block[
Kuhn, M. (2022). #emph[Tidyposterior: Bayesian Analysis to Compare Models using Resampling Statistics].

] <ref-kuhnTidyposteriorBayesianAnalysis2022>
#block[
Kuhn, M., & Wickham, H. (2020). #emph[Tidymodels: A collection of packages for modeling and machine learning using tidyverse principles].

] <ref-kuhnTidymodelsCollectionPackages2020>
#block[
Lofaro, R. J., Bohler, R. M., Spurgeon, R., & Mase, W. A. (2025). Rural and urban differences in treatment on demand for substance use treatment involving medications for opioid use disorder. #emph[The Journal of Rural Health], #emph[41]\(3), e70076. #link("https://doi.org/10.1111/jrh.70076")

] <ref-lofaroRuralUrbanDifferences2025>
#block[
Lundberg, S. M., & Lee, S.-I. (2017). A unified approach to interpreting model predictions. #emph[Proceedings of the 31st International Conference on Neural Information Processing Systems, NIPS'17], 4768--4777.

] <ref-lundbergUnifiedApproachInterpreting2017>
#block[
Ma, J., Bao, Y.-P., Wang, R.-J., Su, M.-F., Liu, M.-X., Li, J.-Q., Degenhardt, L., Farrell, M., Blow, F. C., Ilgen, M., Shi, J., & Lu, L. (2019). Effects of medication-assisted treatment on mortality among opioids users: A systematic review and meta-analysis. #emph[Molecular Psychiatry], #emph[24]\(12), 1868--1883. #link("https://doi.org/10.1038/s41380-018-0094-5")

] <ref-maEffectsMedicationassistedTreatment2019>
#block[
Martin, C. E., Parlier-Ahmad, A. B., Beck, L., Scialli, A., & Terplan, M. (2022). Need for and Receipt of Substance Use Disorder Treatment Among Adults, by Gender, in the United States. #emph[Public Health Reports®], #emph[137]\(5), 955--963. #link("https://doi.org/10.1177/00333549211041554")

] <ref-martinNeedReceiptSubstance2022>
#block[
Mohr, D. C., Zhang, M., & Schueller, S. M. (2017). Personal Sensing: Understanding Mental Health Using Ubiquitous Sensors and Machine Learning. #emph[Annual Review of Clinical Psychology], #emph[13]\(1), 23--47. #link("https://doi.org/10.1146/annurev-clinpsy-032816-044949")

] <ref-mohrPersonalSensingUnderstanding2017>
#block[
Molnar, C. (2022). #emph[Interpretable Machine Learning: A Guide For Making Black Box Models Explainable]. Independently published.

] <ref-molnarInterpretableMachineLearning2022>
#block[
Moshontz, H., Colmenares, A. J., Fronk, G. E., Sant'Ana, S. J., Wyant, K., Wanta, S. E., Maus, A., Jr, D. H. G., Shah, D., & Curtin, J. J. (2021). Prospective Prediction of Lapses in Opioid Use Disorder: Protocol for a Personal Sensing Study. #emph[JMIR Research Protocols], #emph[10]\(12), e29563. #link("https://doi.org/10.2196/29563")

] <ref-moshontzProspectivePredictionLapses2021>
#block[
Olfson, M., Mauro, C., Wall, M. M., Choi, C. J., Barry, C. L., & Mojtabai, R. (2022). Healthcare coverage and service access for low-income adults with substance use disorders. #emph[Journal of Substance Abuse Treatment], #emph[137], 108710. #link("https://doi.org/10.1016/j.jsat.2021.108710")

] <ref-olfsonHealthcareCoverageService2022>
#block[
Pinedo, M. (2019). A current re-examination of racial/ethnic disparities in the use of substance abuse treatment: Do disparities persist? #emph[Drug and Alcohol Dependence], #emph[202], 162--167. #link("https://doi.org/10.1016/j.drugalcdep.2019.05.017")

] <ref-pinedoCurrentReexaminationRacial2019>
#block[
Platt, J. (1999). Probabilistic outputs for support vector machines and comparisons to regularized likelihood methods. In #emph[Advances in large margin classifiers] (pp. 61--74). MIT Press.

] <ref-plattProbabilisticOutputsSupport1999>
#block[
Substance Abuse and Mental Health Services Administration. (n.d.). #emph[2024 NSDUH Detailed Tables CBHSQ Data]. https:\/\/www.samhsa.gov/data/report/2024-nsduh-detailed-tables.

] <ref-substanceabuseandmentalhealthservicesadministration2024NSDUHDetailed>
#block[
Van Calster, B., Collins, G. S., Vickers, A. J., Wynants, L., Kerr, K. F., Barreñada, L., Varoquaux, G., Singh, K., Moons, K. G., Hernandez-Boussard, T., Timmerman, D., McLernon, D. J., Van Smeden, M., & Steyerberg, E. W. (2025). Evaluation of performance measures in predictive artificial intelligence models to support medical decisions: Overview and guidance. #emph[The Lancet Digital Health], 100916. #link("https://doi.org/10.1016/j.landig.2025.100916")

] <ref-vancalsterEvaluationPerformanceMeasures2025>
#block[
Vandewiele, G., Dehaene, I., Kovács, G., Sterckx, L., Janssens, O., Ongenae, F., De Backere, F., De Turck, F., Roelens, K., Decruyenaere, J., Van Hoecke, S., & Demeester, T. (2021). Overly optimistic prediction results on imbalanced data: A case study of flaws and benefits when applying over-sampling. #emph[Artificial Intelligence in Medicine], #emph[111], 101987. #link("https://doi.org/10.1016/j.artmed.2020.101987")

] <ref-vandewieleOverlyOptimisticPrediction2021>
#block[
Weinstein, Z. M., Kim, H. W., Cheng, D. M., Quinn, E., Hui, D., Labelle, C. T., Drainoni, M.-L., Bachman, S. S., & Samet, J. H. (2017). Long-term retention in Office Based Opioid Treatment with buprenorphine. #emph[Journal of Substance Abuse Treatment], #emph[74], 65--70. #link("https://doi.org/10.1016/j.jsat.2016.12.010")

] <ref-weinsteinLongtermRetentionOffice2017>
#block[
Wyant, K., Moshontz, H., Ward, S. B., Fronk, G. E., & Curtin, J. J. (2023). Acceptability of Personal Sensing Among People With Alcohol Use Disorder: Observational Study. #emph[JMIR mHealth and uHealth], #emph[11]\(1), e41833. #link("https://doi.org/10.2196/41833")

] <ref-wyantAcceptabilityPersonalSensing2023>
#block[
Wyant, K., Sant'Ana, S. J. K., Fronk, G., & Curtin, J. J. (2024). Machine learning models for temporally precise lapse prediction in alcohol use disorder. #emph[Psychopathology and Clinical Science]. #link("https://doi.org/10.31234/osf.io/cgsf7")

] <ref-wyantMachineLearningModels2024>
#block[
Xin, Y., Schwarting, C. M., Wasef, M. R., & Davis, A. K. (2023). Exploring the intersectionality of stigma and substance use help-seeking behaviours among lesbian, gay, bisexual, transgender, queer, questioning or otherwise gender or sexuality minority (LGBTQ+) individuals in the United States: A scoping review. #emph[Global Public Health], #emph[18]\(1), 2277854. #link("https://doi.org/10.1080/17441692.2023.2277854")

] <ref-xinExploringIntersectionalityStigma2023>
] <refs>




# DemoChemMapping
Mock models for CCPP-framework mapping question


**NOTE:** This is a *demonstration* of the *types* of logic used to build maps between chemical mechanisms, emissions modules, photolysis modules, etc.  It is not a science code base.

There are three folders, each of which demonstrates an approach to the mapping:

| Folder                 | Model Description                                                           |
|------------------------| ----------------------------------------------------------------------------|
| `how_things_are/`      | [COMPLETE] A rough approximation of how current models might handle mapping |
| `how_things_could_be/` | [COMPLETE] One approach to improving sustainability over the `how_things_are` approach |
| `how_things_must_be/`  | [UNFINISHED] A mock model addressing the code sustainability issues described below that uses CCPP-framework |

Each of the models can be built and run from their folder with:

```
mkdir build
cd build
cmake ..
make
./host_model
```

There are challenges related to the mapping of namespaces between model components. We are using this code to try to explicitly describe what these are. (They are labelled **CH#** in the following discussion.) Similarly, sustainability issues exist with current model designs that we are trying to address while developing the MUSICA infrastructure. (These are labelled **SI#** below.) Features of the `how_things_could_be` model that address the challenges and sustainability issues are labelled **F#**.

## Scenario

All three models run the same 8 scenarios, which are all possible combinations of two imaginary chemical mechanisms (`QX5` and `QXZ`), two imaginary emissions modules (`MARGE` and `ARES`), and two photolysis modules (`TUV` and `FastJ`).

| Scenario | Chemical Mechanism | Emissions | Photolysis |
|----------|--------------------|-----------|------------|
| 1        | QX5                | MARGE     | TUV        |
| 2        | QX5                | MARGE     | FastJ      |
| 3        | QX5                | ARES      | TUV        |
| 4        | QX5                | ARES      | FastJ      |
| 1        | QXZ                | MARGE     | TUV        |
| 2        | QXZ                | MARGE     | FastJ      |
| 3        | QXZ                | ARES      | TUV        |
| 4        | QXZ                | ARES      | FastJ      |

Each imaginary chemical mechanism has a unique set of lumped and explicit chemical species and photolysis reactions.

### Mechanism Species

#### QX5

| Species | Type     | Description                             |
|---------|----------|-----------------------------------------|
| ETHENE  | Explicit | H2C=CH2                                 |
| PROPENE | Explicit | H3C-CH=CH2                              |
| BIGALK  | Lumped   | Any alkene with more than 3 carbons     |

#### QXZ

| Species | Type     | Description                                                              |
|---------|----------|--------------------------------------------------------------------------|
| ETHENE  | Explicit | H2C=CH2                                                                  |
| TALK    | Lumped   | Any terminal alkene (-CH=CH2) with less than 7 carbons that isn't ethene |
| IALK    | Lumped   | Any interal alkene (-CH=CH-) with less than 7 carbons                    |
| BIGALK  | Lumped   | Any alkene with 7 or more carbons                                        |

**CH1** The same species name may mean different things in different mechanisms.

In our example, `BIGALK` is alkenes with more than 3 carbons in `QX5` and is alkenes with more than 6 carbons in `QXZ`. These differences can exist between different mechanisms and even between different versions of the same mechanism.

**CH2** Adding a chemical species to a mechanism may implicitly change the definition of other species.

In our example, adding butene to the `QX5` mechanism implicitly changes the meaning of `BIGALK` to be alkenes with more than 4 carbons. Adding monoterpenes (a class of biogenic alkenes) would change `BIGALK` to someting that is even harder to explicitly define.

### Emissions

Emissions models can have more explicit chemical representation than the mechanisms used in large models. In our example, the `MARGE` module focuses on biogenic emissions and the `ARES` module focuses on anthropogenic emissions. The mapping between emissions and mechanisms is tightly tied to how `QX5` and `QXZ` define their various species and how emitted species are defined in each emissions module.

**CH3** The definition of what a lumped chemical species is in a chemical mechanism makes a big difference in how other modules interact with it.

The logic used to map among emissions schemes and chemical mechanisms is in comments in the `how_things_are` source code with the mapping:

| Emissions Module | source code with mapping description     |
| -----------------| -----------------------------------------|
| MARGE            | `how_things_are/src/emissions_marge.F90` |
| ARES             | `how_things_are/src/emissions_ares.F90`  |


**SI1** Researchers adding a new species to a chemical mechanism may not realize that as they implicitly change the meaning of other chemical species (**CH2**) on which other modules rely, they are affecting the way mapping to and from other modules should be. 

This is particularly true when the mapping logic has been fossilized under many layers of Fortran77 sedimentation, as could be imagined for the `how_things_are` model. 

#### Photolysis

In yet another cruel twist of fate, photolysis rates are not tied to specific chemical species, but to specific reactions. For example, ozone undergoes two different photolysis reactions with different products and different rates that are part of most chemical mechanisms:

```
O3 + hv -> O(3P) + O2
O3 + hv -> O(1D) + O2
```

Lumped chemical species are hard to label, but reactions are ridiculously hard to label because every product and reactant and even the rate can be affected by how a particular mechanism defines its lumped species, and on the chemical resolution of the mechanism (e.g., `QX5` may have two sequential reactions that `QX6` treats as a single combined reaction).

It is even feasible that the mapping between a photolysis module would depend on what emissions module is used, as the chemical resolution of a chemical mechanism is sometimes lower than emissions or photolysis modules. An example of how photolysis mapping could change when you change the emissions scheme is described in the comments in:

| Photolysis Module | source code with mapping description      |
|-------------------|-------------------------------------------|
| TUV               | `how_things_are/src/photolysis_tuv.F90`   |
| FastJ             | `how_things_are/src/photolysis_fastj.F90` |

**CH4** The mapping between independent modules (e.g., photolysis and emissions) and chemical mechanisms may be inter-related.

## How things are

The `how_things_are` model gives an example of mapping in a hard-coded model that can run our 8 scenarios. 

In addition to issue **SI1** (the implicit changes to mapping when adding new chemical species), there are several other sustainability issues related to this approach.

**SI2** Adding a new module can involve changing the source code of other modules and the host model.

In our `how_things_are` example, adding a new photolysis module involves changing:

| Files changed when adding a new photolysis module |
|---------------------------------------------------|
| `how_things_are/src/emissions_marge.F90`          |
| `how_things_are/src/emissions_ares.F90`           |
| `how_things_are/src/chemical_mechanism_qx5.F90`   |
| `how_things_are/src/chemical_mechanism_qxz.F90`   |
| `how_things_are/src/photolysis_fastj.F90`         |
| `how_things_are/src/host_model.F90`               |
| `how_things_are/src/host_model_data.F90`          |

**SI3** When adding a new module, you must immediately map between every possible scenario in which it could participate, or add code to fail the run saying something like "these things don't work together."

In our `how_things_are` model, adding a photolysis module would involve 4 such scenarios; in a real model, this number could be much higher.

## How things could be

The `how_things_could_be` model demonstrates one way that the challenges (**CH1-4**) and sustainability issues (**S1-3**) could be addressed.

**F1** There is no global naming scheme.

**F2** Mapping between local namespaces is responsible for transferring information among the various model components.

The lack of a global naming scheme (**F1**) and the ability to map between local namespaces (**F2**) addresses challenge **CH1**.

**F3** Mapping is part of the run-time configuration of the model.

This addesses challenges **CH2-4** and **SI1** as configuration files are much more visible to users, and can be documented to make clear what assumptions are made in mapping between model components. (The `config/*.map` files of the `how_things_could_be` model would, in practice, include the ability to include comments for each mapping entry that can describe the logic used by the scientist who developed it.)

Challenge **CH4** is also addressed by the ability to provide maps between model components that are depenedent on other model components when needed. In our example, the mapping between photolysis modules and chemical mechanisms depends on assumptions made in the mapping of emissions to the mechanism species. This is captured by:

| Specialized mapping files |
|---------------------------|
| `how_things_could_be/config/qx5_fastj_with_ares.map`  |
| `how_things_could_be/config/qx5_fastj_with_marge.map` |
| `how_things_could_be/config/qxz_fastj_with_ares.map`  |
| `how_things_could_be/config/qxz_fastj_with_marge.map` |

**F4** Model components are abstracted.

An abstract design for model components reduces the changes to model code required when adding a new module (**SI2**). When adding a new photolysis module to the `how_things_could_be` model, only one file would need to be modified:

| Files changed when adding a new photolysis module |
|---------------------------------------------------|
| `how_things_could_be/src/photolysis_factory.F90`  |

This feature along with **F3** also addresses **SI3**, as a map must be provided at run-time, precluding the need to hard-code logic related what to do in every possible combination of components every time a new component is added.

**F5** Bonus feature - this design is entirely compatible with a fully run-time configuration of every model component.

## How things must be

The `how_things_must_be` model is where we want to build a mock model that addresses the challenges and sustainability issues (and ideally include the features of the `how_things_could_be` model) using the CCPP-framework.




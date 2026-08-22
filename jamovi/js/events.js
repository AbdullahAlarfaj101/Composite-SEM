'use strict';

const events = {

    // UPDATED in 1.7: The global update pass now also tracks changes to the
    // higher-order construct list, refreshes the moderation selectors, and
    // re-evaluates whether the Multi-Group Analysis options may be enabled.
    update: function(ui) {
        try {
            updateModelLabels(ui);

            var latentChanges = this.findChanges("latent", ui.latent.value(), true);
            var compositeChanges = this.findChanges("composite", ui.composite.value(), true);
            var hocChanges = ui.hoc ? this.findChanges("hoc", ui.hoc.value(), true) : { hasChanged: false };
            var endogenousChanges = this.findChanges("endogenousClass", ui.endogenousClass.value(), true);
            var exogenousChanges = this.findChanges("exogenousClass", ui.exogenousClass.value(), true);
            var endogenousTermsChanges = this.findChanges("endogenousTerms", ui.endogenousTerms.value(), true);

            if (latentChanges.hasChanged || compositeChanges.hasChanged || hocChanges.hasChanged) {
                updateSuppliers(ui, this);
                cleanRoleSelections(ui, this);
                cleanPathTerms(ui, this);
            } else if (endogenousChanges.hasChanged || exogenousChanges.hasChanged) {
                cleanRoleSelections(ui, this);
                cleanPathTerms(ui, this);
            } else if (endogenousTermsChanges.hasChanged) {
                cleanPathTerms(ui, this);
            }
            updateModes(ui, this);
            updateModerationDropdowns(ui, this);
            updateMultigroupOptions(ui);
        } catch (err) {
            if (ui.debugLabel) {
                ui.debugLabel.setPropertyValue('label', 'Error in update: ' + err.message + '\nStack: ' + err.stack);
            }
        }
    },



    onChange_roleAssignments: function(ui) {
        try {
            cleanRoleSelections(ui, this);
            cleanPathTerms(ui, this);
        } catch (err) {
            if (ui.debugLabel) {
                ui.debugLabel.setPropertyValue('label', 'Error in onChange_roleAssignments: ' + err.message + '\nStack: ' + err.stack);
            }
        }
    },

    onChange_endogenousTerms: function(ui) {
        try {
            cleanPathTerms(ui, this);
        } catch (err) {
            if (ui.debugLabel) {
                ui.debugLabel.setPropertyValue('label', 'Error in onChange_endogenousTerms: ' + err.message + '\nStack: ' + err.stack);
            }
        }
    },

    // NEW in 1.7: Enable or disable the Multi-Group Analysis test selectors
    // as soon as the grouping variable is assigned or cleared, so MGA methods
    // cannot be requested for a model without groups.
    onChange_multg: function(ui) {
        updateMultigroupOptions(ui);
    },

    onChange_roleSupplier: function(ui) {
    },

    onUpdate_roleSupplier: function(ui) {
        try {
            updateSuppliers(ui, this);
        } catch (err) {
            if (ui.debugLabel) {
                ui.debugLabel.setPropertyValue('label', 'Error in onUpdate_roleSupplier: ' + err.message + '\nStack: ' + err.stack);
            }
        }
    },

    // NEW in 1.7: Higher-Order Constructs supplier hooks. The HOC supplier is
    // fed with the lower-order constructs (LOCs) only, so a higher-order
    // construct can never be dragged into itself or into another HOC.
    onChange_hocSupplier: function(ui) {
    },

    onUpdate_hocSupplier: function(ui) {
        try {
            updateSuppliers(ui, this);
        } catch (err) {
            if (ui.debugLabel) {
                ui.debugLabel.setPropertyValue('label', 'Error in onUpdate_hocSupplier: ' + err.message + '\nStack: ' + err.stack);
            }
        }
    },

    onChange_pathSupplier: function(ui) {
    },

    onUpdate_pathSupplier: function(ui) {
        try {
            updateSuppliers(ui, this);
        } catch (err) {
            if (ui.debugLabel) {
                ui.debugLabel.setPropertyValue('label', 'Error in onUpdate_pathSupplier: ' + err.message + '\nStack: ' + err.stack);
            }
        }
    },

    // UPDATED in 1.7: Adding or removing an item now forces a supplier refresh
    // (force = true) instead of relying on the array comparison, because the
    // HOC supplier holds a different set of labels than the role/path
    // suppliers and would otherwise be left stale. The moderation selectors
    // are refreshed in the same pass.
    onChange_items_changed: function(ui) {
        try {
            updateModelLabels(ui);
            updateSuppliers(ui, this, true);
            cleanRoleSelections(ui, this);
            cleanPathTerms(ui, this);
            updateModes(ui, this);
            updateModerationDropdowns(ui, this);
        } catch (err) {
            if (ui.debugLabel) {
                ui.debugLabel.setPropertyValue('label', 'Error in onChange_items_changed: ' + err.message + '\nStack: ' + err.stack);
            }
        }
    },

    // UPDATED in 1.7: Renaming a construct forces a supplier refresh as well,
    // so a renamed lower-order construct is propagated to the HOC supplier and
    // to the moderation selectors immediately.
    onChange_constructName: function(ui) {
        try {
            updateSuppliers(ui, this, true);
            cleanRoleSelections(ui, this);
            cleanPathTerms(ui, this);
            updateModes(ui, this);
            updateModerationDropdowns(ui, this);
        } catch (err) {
            if (ui.debugLabel) {
                ui.debugLabel.setPropertyValue('label', 'Error in onChange_constructName: ' + err.message + '\nStack: ' + err.stack);
            }
        }
    },

    // UPDATED in 1.5: When the estimation method changes, refresh the
    // per-construct weighting mode list so the Mode A / Mode B selectors are
    // only enabled while PLS estimation is active.
    onChange_alt: function(ui) {
        try {
            updateModes(ui, this);
        } catch (err) {
            if (ui.debugLabel) {
                ui.debugLabel.setPropertyValue('label', 'Error in onChange_alt: ' + err.message + '\nStack: ' + err.stack);
            }
        }
    },

    // NEW in 1.5: When the path diagram is (re-)enabled, switch estimate
    // labels back on so the plot is informative by default.
    // UPDATED in 1.7: Significance stars are switched on as well, so a freshly
    // enabled diagram immediately reports both the estimates and their
    // significance.
    onChange_showPlot: function(ui) {
        try {
            if (ui.showPlot.value() === true) {
                ui.showEstimates.setValue(true);
                ui.showSigStars.setValue(true);
            }
        } catch (err) {
            if (ui.debugLabel) {
                ui.debugLabel.setPropertyValue('label', 'Error in onChange_showPlot: ' + err.message + '\nStack: ' + err.stack);
            }
        }
    },

    // NEW in 1.7: Cascade the moderation selectors. Changing the dependent
    // construct rebuilds the list of admissible independent constructs, and
    // changing the independent construct rebuilds the list of admissible
    // moderators, so the three selections always describe a valid interaction.
    onChange_modDependent: function(ui) {
        try {
            updateModerationDropdowns(ui, this);
        } catch (err) {}
    },

    onChange_modIndependent: function(ui) {
        try {
            updateModerationDropdowns(ui, this);
        } catch (err) {}
    },

    // NEW in 1.7: When moderation analysis is switched on, show the simple
    // effects table by default and populate the three construct selectors
    // straight away, so the panel is immediately usable.
    onChange_moderationEnabled: function(ui) {
        try {
            if (ui.moderationEnabled.value() === true) {
                if (ui.showSimpleEffectsTable) {
                    ui.showSimpleEffectsTable.setValue(true);
                }
                updateModerationDropdowns(ui, this);
            }
        } catch (err) {
            if (ui.debugLabel) {
                ui.debugLabel.setPropertyValue('label', 'Error in onChange_moderationEnabled: ' + err.message + '\nStack: ' + err.stack);
            }
        }
    }
};

function getLabels(items) {
    var labels = [];

    for (var i = 0; i < items.length; i++) {
        var item = items[i];
        if (!item || !item.label || item.label === '')
            continue;
        if (!item.vars || item.vars.length === 0)
            continue;
        labels.push(item.label);
    }

    return labels;
}

// NEW in 1.7: Returns the labels of the lower-order constructs (LOCs) only,
// i.e. the reflective latent and formative composite constructs. This is the
// pool offered by the HOC supplier and is deliberately kept separate from
// allConstructLabels(), which also contains the higher-order constructs.
function locLabels(ui, context) {
    var latentItems = context.cloneArray(ui.latent.value(), []);
    var compositeItems = context.cloneArray(ui.composite.value(), []);
    return getLabels(latentItems).concat(getLabels(compositeItems));
}

function updateModelLabels(ui) {
    ui.latent.applyToItems(0, function(item, index) {
        var value = item.controls[0].value();
        if (!value || value.trim() === '')
            item.controls[0].setValue('Latent' + (index + 1));
    });

    ui.composite.applyToItems(0, function(item, index) {
        var value = item.controls[0].value();
        if (!value || value.trim() === '')
            item.controls[0].setValue('Composite' + (index + 1));
    });

    // NEW in 1.7: Give unnamed higher-order constructs a default 'HOC<n>'
    // label, mirroring the 'Latent<n>' / 'Composite<n>' defaults above.
    if (ui.hoc) {
        ui.hoc.applyToItems(0, function(item, index) {
            var value = item.controls[0].value();
            if (!value || value.trim() === '')
                item.controls[0].setValue('HOC' + (index + 1));
        });
    }
}

// NEW in 1.7: Collects the labels of the higher-order constructs that are
// actually usable, i.e. those that already have at least one lower-order
// component assigned. Unnamed entries fall back to the auto-generated
// 'HOC<n>' label used by updateModelLabels().
function getHOCLabels(items) {
    var labels = [];
    if (!items) return labels;
    for (var i = 0; i < items.length; i++) {
        var item = items[i];
        if (!item) continue;
        var label = (item.label && item.label.trim() !== '') ? item.label : ('HOC' + (i + 1));
        if (!item.components || item.components.length === 0)
            continue;
        labels.push(label);
    }
    return labels;
}

// UPDATED in 1.7: The construct pool used throughout the interface (roles,
// directional paths, moderation) now also contains the higher-order
// constructs, so a HOC can be assigned a structural role exactly like an
// ordinary construct.
function allConstructLabels(ui, context) {
    var locs = locLabels(ui, context);
    if (ui.hoc) {
        var hocItems = context.cloneArray(ui.hoc.value(), []);
        return locs.concat(getHOCLabels(hocItems));
    }
    return locs;
}

function arraysEqual(a, b) {
    if (a === b) return true;
    if (a == null || b == null) return false;
    if (a.length !== b.length) return false;
    for (var i = 0; i < a.length; ++i) {
        if (a[i] !== b[i]) return false;
    }
    return true;
}

// UPDATED in 1.7: Also keeps the HOC supplier synchronised. The role and path
// suppliers receive the full construct pool (LOCs + HOCs), while the HOC
// supplier receives the lower-order constructs only. Every supplier lookup is
// guarded so the function stays safe when a panel is not present in the UI.
function updateSuppliers(ui, context, force) {
    try {
        var locs = locLabels(ui, context);
        var labels = allConstructLabels(ui, context);
        
        var roleSupplierVal = ui.roleSupplier ? ui.roleSupplier.value() : null;
        var pathSupplierVal = ui.pathSupplier ? ui.pathSupplier.value() : null;
        var hocSupplierVal  = ui.hocSupplier ? ui.hocSupplier.value() : null;

        var currentRoles = roleSupplierVal ? context.itemsToValues(roleSupplierVal) : [];
        var currentPaths = pathSupplierVal ? context.itemsToValues(pathSupplierVal) : [];
        var currentHOCs  = hocSupplierVal ? context.itemsToValues(hocSupplierVal) : [];

        var expectedHOCs = locs;

        if (!arraysEqual(labels, currentRoles) || !arraysEqual(labels, currentPaths) || !arraysEqual(expectedHOCs, currentHOCs) || force) {
            var customVariables = [];
            for (var i = 0; i < labels.length; i++) {
                customVariables.push({
                    name: labels[i],
                    measureType: 'none',
                    dataType: 'none',
                    levels: []
                });
            }
            context.setCustomVariables(customVariables);
            
            var items = context.valuesToItems(labels, FormatDef.variable);
            var hocItems = context.valuesToItems(expectedHOCs, FormatDef.variable);

            if (ui.roleSupplier) ui.roleSupplier.setValue(items);
            if (ui.pathSupplier) ui.pathSupplier.setValue(items);
            if (ui.hocSupplier) ui.hocSupplier.setValue(hocItems);
        }
    } catch (err) {
        throw err;
    }
}

function cleanRoleSelections(ui, context) {
    var labels = allConstructLabels(ui, context);
    var endogenous = cleanTermList(context.cloneArray(ui.endogenousClass.value(), []), labels, context);
    var exogenous = cleanTermList(context.cloneArray(ui.exogenousClass.value(), []), labels, context);

    var endogenousNames = endogenous.map(termToString);
    exogenous = exogenous.filter(function(term) {
        return endogenousNames.indexOf(termToString(term)) < 0;
    });

    var endogenousChanges = context.findChanges("cleanEndogenousClass", endogenous, true);
    var exogenousChanges = context.findChanges("cleanExogenousClass", exogenous, true);

    if (endogenousChanges.hasChanged) {
        ui.endogenousClass.setValue(endogenous);
    }
    if (exogenousChanges.hasChanged) {
        ui.exogenousClass.setValue(exogenous);
    }
}

function preparePathTerms(ui, context) {
    var endogenous = orderedEndogenousLabels(ui, context);
    var endogenousTerms = context.cloneArray(ui.endogenousTerms.value(), []);
    var okList = [];

    var listChanged = false;
    for (var i = 0; i < endogenous.length; i++) {
        var term = endogenousTerms[i] === undefined ? [] : endogenousTerms[i];
        okList.push(term);
        if (endogenousTerms[i] === undefined) {
            listChanged = true;
        }
    }
    
    if (endogenousTerms.length !== okList.length) {
        listChanged = true;
    }

    if (listChanged) {
        ui.endogenousTerms.setValue(okList);
    }
    labelize(ui.endogenousTerms, endogenous, 'Endogenous');
}

function cleanPathTerms(ui, context) {
    preparePathTerms(ui, context);

    var allLabels = allConstructLabels(ui, context);
    var endogenous = orderedEndogenousLabels(ui, context);
    var endogenousTerms = context.cloneArray(ui.endogenousTerms.value(), []);

    var termsChanged = false;
    for (var i = 0; i < endogenous.length; i++) {
        var cleaned = cleanTermList(endogenousTerms[i], allLabels, context);
        cleaned = cleaned.filter(function(term) {
            return termToString(term) !== endogenous[i];
        });
        
        var termStrOld = (endogenousTerms[i] || []).map(termToString).join(",");
        var termStrNew = cleaned.map(termToString).join(",");
        if (termStrOld !== termStrNew) {
            endogenousTerms[i] = cleaned;
            termsChanged = true;
        }
    }

    if (termsChanged) {
        ui.endogenousTerms.setValue(endogenousTerms);
    }
}

function orderedEndogenousLabels(ui, context) {
    var labels = allConstructLabels(ui, context);
    var endogenous = cleanTermList(context.cloneArray(ui.endogenousClass.value(), []), labels, context)
        .map(termToString);

    return labels.filter(function(label) {
        return endogenous.indexOf(label) >= 0;
    });
}

function cleanTermList(list, validLabels, context) {
    var terms = normalize(context.cloneArray(list, []));
    var seen = {};
    var cleaned = [];

    for (var i = 0; i < terms.length; i++) {
        var text = termToString(terms[i]);
        if (text === '' || validLabels.indexOf(text) < 0 || seen[text])
            continue;
        seen[text] = true;
        cleaned.push(terms[i]);
    }

    return cleaned;
}

function termToString(value) {
    if (value === undefined || value === null)
        return '';
    if (Array.isArray(value))
        return value.join(':');
    if (typeof value === 'object') {
        if (Array.isArray(value.components))
            return value.components.join(':');
        if (value.name)
            return String(value.name);
    }
    return String(value);
}

function labelize(widget, labels, prefix) {
    widget.applyToItems(0, function(item, index) {
        item.controls[0].setPropertyValue('label', prefix + ' = ' + labels[index]);
    });
}

function normalize(value) {
    if (value === undefined || value === null)
        return [];
    return Array.isArray(value) ? value : [ value ];
}

// NEW in 1.5: Keeps the 'modes' option (per-construct PLS weighting modes)
// in sync with the composites defined in the UI. Newly added composites are
// appended with Mode B as the default weighting scheme, removed composites
// are dropped, and existing selections are preserved. The mode selectors are
// enabled only when PLS is the active estimation method, since cSEM's
// .PLS_modes argument applies exclusively to PLS weighting.
function updateModes(ui, context) {
    var compositeItems = context.cloneArray(ui.composite.value(), []);
    var compositeLabels = getLabels(compositeItems);
    
    var currentModes = context.cloneArray(ui.modes.value(), []);
    var newModes = [];
    var changed = false;
    
    for (var i = 0; i < compositeLabels.length; i++) {
        var label = compositeLabels[i];
        var existing = null;
        for (var j = 0; j < currentModes.length; j++) {
            if (currentModes[j] && currentModes[j].construct === label) {
                existing = currentModes[j];
                break;
            }
        }
        
        if (existing) {
            newModes.push(existing);
        } else {
            newModes.push({
                construct: label,
                mode: "modeB"
            });
            changed = true;
        }
    }
    
    if (currentModes.length !== newModes.length) {
        changed = true;
    } else {
        for (var i = 0; i < newModes.length; i++) {
            if (currentModes[i].construct !== newModes[i].construct || currentModes[i].mode !== newModes[i].mode) {
                changed = true;
                break;
            }
        }
    }
    
    if (changed) {
        ui.modes.setValue(newModes);
    }
    
    var estimationMethod = ui.alt.value();
    var isPLS = (estimationMethod === "PLS");
    var newOptions = [
        { name: "modeA", title: "Mode A" },
        { name: "modeB", title: "Mode B" }
    ];

    ui.modes.applyToItems(0, function(item, index) {
        if (index < compositeLabels.length) {
            item.controls[0].setPropertyValue('label', compositeLabels[index]);
            item.controls[1].setPropertyValue('options', newOptions);
            item.controls[1].setPropertyValue('enable', isPLS);
        }
    });
}

// NEW in 1.7: Populates the three moderation selectors from the specified
// structural model and keeps them mutually consistent:
//   1. Dependent (Y)   - endogenous constructs only (all constructs when no
//                        structural roles have been assigned yet).
//   2. Independent (X) - the predictors declared for the selected Y in the
//                        directional path blocks, excluding interaction terms.
//   3. Moderator (M)   - those same predictors minus the selected X.
// Whenever a selection becomes invalid after a model change it is silently
// reset to the first admissible construct, so the panel can never describe an
// interaction that is not estimable in the current model.
function updateModerationDropdowns(ui, context) {
    try {
        var allLabels = allConstructLabels(ui, context);
        var endogenousLabels = orderedEndogenousLabels(ui, context);
        
        // 1. Dependent Construct (Y) options: ONLY Endogenous constructs!
        var depLabels = (endogenousLabels && endogenousLabels.length > 0) ? endogenousLabels : allLabels;
        var depOptions = [{ name: "", title: "(Select Construct)" }];
        for (var i = 0; i < depLabels.length; i++) {
            depOptions.push({ name: depLabels[i], title: depLabels[i] });
        }
        if (ui.modDependent) {
            ui.modDependent.setPropertyValue('options', depOptions);
        }
        
        var selectedDep = ui.modDependent ? ui.modDependent.value() : "";
        if ((!selectedDep || depLabels.indexOf(selectedDep) < 0) && depLabels.length > 0) {
            selectedDep = depLabels[0];
            if (ui.modDependent) ui.modDependent.setValue(selectedDep);
        }
        
        // 2. Find predictors for selectedDep from endogenousTerms
        var predictors = [];
        if (selectedDep) {
            var endoTerms = context.cloneArray(ui.endogenousTerms.value(), []);
            var endoList = endogenousLabels;
            var depIdx = endoList.indexOf(selectedDep);
            if (depIdx >= 0 && depIdx < endoTerms.length && endoTerms[depIdx]) {
                var termList = cleanTermList(endoTerms[depIdx], allLabels, context);
                for (var k = 0; k < termList.length; k++) {
                    var termStr = termToString(termList[k]);
                    if (termStr && termStr !== selectedDep && termStr.indexOf(':') < 0 && termStr.indexOf('.') < 0) {
                        predictors.push(termStr);
                    }
                }
            }
        }
        
        if (predictors.length === 0) {
            predictors = allLabels.filter(function(lbl) { return lbl !== selectedDep; });
        }
        
        // 3. Independent Construct (X) options: Predictors of Y
        var indOptions = [{ name: "", title: "(Select Construct)" }];
        for (var j = 0; j < predictors.length; j++) {
            indOptions.push({ name: predictors[j], title: predictors[j] });
        }
        if (ui.modIndependent) {
            ui.modIndependent.setPropertyValue('options', indOptions);
        }
        
        var selectedInd = ui.modIndependent ? ui.modIndependent.value() : "";
        if ((!selectedInd || predictors.indexOf(selectedInd) < 0) && predictors.length > 0) {
            selectedInd = predictors[0];
            if (ui.modIndependent) ui.modIndependent.setValue(selectedInd);
        }
        
        // 4. Moderator Construct (M) options: Predictors of Y excluding X
        var modPredictors = predictors.filter(function(p) { return p !== selectedInd; });
        var modOptions = [{ name: "", title: "(Select Construct)" }];
        for (var m = 0; m < modPredictors.length; m++) {
            modOptions.push({ name: modPredictors[m], title: modPredictors[m] });
        }
        if (ui.modModerator) {
            ui.modModerator.setPropertyValue('options', modOptions);
        }
        
        var selectedMod = ui.modModerator ? ui.modModerator.value() : "";
        if ((!selectedMod || selectedMod === selectedInd || modPredictors.indexOf(selectedMod) < 0) && modPredictors.length > 0) {
            selectedMod = modPredictors[0];
            if (ui.modModerator) ui.modModerator.setValue(selectedMod);
        }
    } catch (err) {
    }
}

// NEW in 1.7: Enables the Multi-Group Analysis test selectors only while a
// grouping variable is assigned. The grouping variable may be delivered by
// jamovi as an array, a plain string or an item object, so all three shapes
// are inspected before deciding.
function updateMultigroupOptions(ui) {
    try {
        var val = ui.multg ? ui.multg.value() : null;
        var hasMultg = false;
        if (Array.isArray(val)) {
            hasMultg = val.length > 0 && val[0] !== "" && val[0] !== null;
        } else if (typeof val === "string") {
            hasMultg = val.trim().length > 0;
        } else if (val && typeof val === "object") {
            hasMultg = !!(val.name || val.value);
        } else if (val) {
            hasMultg = true;
        }

        if (ui.mgaHenseler) ui.mgaHenseler.setPropertyValue('enable', hasMultg);
        if (ui.mgaSarstedt) ui.mgaSarstedt.setPropertyValue('enable', hasMultg);
        if (ui.mgaChin) ui.mgaChin.setPropertyValue('enable', hasMultg);
        if (ui.mgaKeil) ui.mgaKeil.setPropertyValue('enable', hasMultg);
        if (ui.mgaNitzl) ui.mgaNitzl.setPropertyValue('enable', hasMultg);
    } catch (err) {
    }
}

module.exports = events;

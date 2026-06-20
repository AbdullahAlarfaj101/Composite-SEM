'use strict';

const events = {

    update: function(ui) {
        try {
            updateModelLabels(ui);
            
            var latentChanges = this.findChanges("latent", ui.latent.value(), true);
            var compositeChanges = this.findChanges("composite", ui.composite.value(), true);
            var endogenousChanges = this.findChanges("endogenousClass", ui.endogenousClass.value(), true);
            var exogenousChanges = this.findChanges("exogenousClass", ui.exogenousClass.value(), true);
            var endogenousTermsChanges = this.findChanges("endogenousTerms", ui.endogenousTerms.value(), true);

            if (latentChanges.hasChanged || compositeChanges.hasChanged) {
                updateSuppliers(ui, this);
                cleanRoleSelections(ui, this);
                cleanPathTerms(ui, this);
            } else if (endogenousChanges.hasChanged || exogenousChanges.hasChanged) {
                cleanRoleSelections(ui, this);
                cleanPathTerms(ui, this);
            } else if (endogenousTermsChanges.hasChanged) {
                cleanPathTerms(ui, this);
            }
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

    onChange_items_changed: function(ui) {
        try {
            updateModelLabels(ui);
            updateSuppliers(ui, this);
            cleanRoleSelections(ui, this);
            cleanPathTerms(ui, this);
        } catch (err) {
            if (ui.debugLabel) {
                ui.debugLabel.setPropertyValue('label', 'Error in onChange_items_changed: ' + err.message + '\nStack: ' + err.stack);
            }
        }
    },

    onChange_constructName: function(ui) {
        try {
            updateSuppliers(ui, this);
            cleanRoleSelections(ui, this);
            cleanPathTerms(ui, this);
        } catch (err) {
            if (ui.debugLabel) {
                ui.debugLabel.setPropertyValue('label', 'Error in onChange_constructName: ' + err.message + '\nStack: ' + err.stack);
            }
        }
    },

    onChange_showPlot: function(ui) {
        try {
            if (ui.showPlot.value() === true) {
                ui.showEstimates.setValue(true);
            }
        } catch (err) {
            if (ui.debugLabel) {
                ui.debugLabel.setPropertyValue('label', 'Error in onChange_showPlot: ' + err.message + '\nStack: ' + err.stack);
            }
        }
    },

    onChange_alt: function(ui) {
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
}

function allConstructLabels(ui, context) {
    var latentItems = context.cloneArray(ui.latent.value(), []);
    var compositeItems = context.cloneArray(ui.composite.value(), []);
    return getLabels(latentItems).concat(getLabels(compositeItems));
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

function updateSuppliers(ui, context, force) {
    try {
        var labels = allConstructLabels(ui, context);
        
        var roleSupplierVal = ui.roleSupplier.value();
        var pathSupplierVal = ui.pathSupplier.value();

        var currentRoles = roleSupplierVal ? context.itemsToValues(roleSupplierVal) : [];
        var currentPaths = pathSupplierVal ? context.itemsToValues(pathSupplierVal) : [];

        if (!arraysEqual(labels, currentRoles) || !arraysEqual(labels, currentPaths) || force) {
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
            ui.roleSupplier.setValue(items);
            ui.pathSupplier.setValue(items);
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

function updateModes(ui, context) {
}

module.exports = events;

